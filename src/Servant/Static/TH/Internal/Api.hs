{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Static.TH.Internal.Api where

import qualified Data.HashTable.IO as H
import Data.List.NonEmpty (NonEmpty)
import Language.Haskell.TH
       (Dec, Q, Type, appT, litT, mkName,
        runIO, strTyLit, tySynD)
import Language.Haskell.TH.Syntax (addDependentFile)
import Servant.API (Get, (:<|>), (:>))
import System.FilePath (takeFileName)

import Servant.Static.TH.Internal.FileTree
import Servant.Static.TH.Internal.Mime
import Servant.Static.TH.Internal.CompressedData (EncodingAwareResponse)

type FileInFilesystem  = FilePath
type CollisionRegistry = H.BasicHashTable VirtualPath FileInFilesystem

fileTreeToApiType :: CollisionRegistry
                  -> FileTree
                  -> Q Type
fileTreeToApiType reg (FileTreeFile filePath _) = do
  HandlerInfo (MimeTypeInfo mimeT respT _) comp virtualPath <- extensionToMimeTypeInfoEx filePath
  maybeCollision <- runIO $ H.lookup reg virtualPath
  case maybeCollision of
    Just x  -> fail (
      "Ambiguous data source for a URL was found: \n\t* "
        <> "URL: "  <> show virtualPath <> " -> "
        <> "File: " <> show filePath    <> " or " <> show x <>
        "\nYou need to remove either of the files, or to move one of them to other directory."
      )
    Nothing -> do
      runIO $ H.insert reg virtualPath filePath
      addDependentFile filePath
      let fileNameLitT = litT $ strTyLit $ takeFileName virtualPath
      [t|$(fileNameLitT) :> Get '[$(mimeT)] (EncodingAwareResponse $(respT))|]
fileTreeToApiType reg (FileTreeDir filePath fileTrees) =
  let fileNameLitT = litT $ strTyLit $ takeFileName filePath
  in [t|$(fileNameLitT) :> $(combineWithServantOrT nonEmptyApiTypesQ)|]
  where
    nonEmptyApiTypesQ :: NonEmpty (Q Type)
    nonEmptyApiTypesQ = fmap (fileTreeToApiType reg) fileTrees

-- | Given a list of @'Q' 'Type'@, combine them with Servant's '(:<|>)'
-- function and return the resulting @'Q' 'Type'@.
combineWithServantOrT :: NonEmpty (Q Type) -> Q Type
combineWithServantOrT = foldl1 $ combineWithType [t|(:<|>)|]

combineWithType :: Q Type -> Q Type -> Q Type -> Q Type
combineWithType combiningType = appT . appT combiningType

-- | Take a template directory argument as a 'FilePath' and create a Servant
-- type representing the files in the directory.  Empty directories will be
-- ignored.
--
-- For example, assume the following directory structure:
--
-- @
--   $ tree dir\/
--   dir\/
--   ├── js
--   │   └── test.js
--   └── index.html
-- @
--
-- 'createApiType' is used like the following:
--
-- @
--   \{\-\# LANGUAGE DataKinds \#\-\}
--   \{\-\# LANGUAGE TemplateHaskell \#\-\}
--
--   type FrontEndAPI = $('createApiType' \"dir\")
-- @
--
-- At compile time, it will expand to the following:
--
-- @
--   type FrontEndAPI =
--          \"js\" ':>' \"test.js\" ':>' 'Get' \'['JS'] 'Data.ByteString.ByteString'
--     ':<|>' \"index.html\" ':>' 'Get' \'['Servant.HTML.Blaze.HTML'] 'Text.Blaze.Html.Html'
-- @
createApiType
  :: FilePath -- ^ directory name to read files from
  -> Q Type
createApiType templateDir = do
  fileTree <- runIO $ getFileTreeIgnoreEmpty templateDir
  reg <- runIO $ H.new
  combineWithServantOrT $ fmap (fileTreeToApiType reg) fileTree

-- | This is similar to 'createApiType', but it creates the whole type synonym
-- declaration.
--
-- Given the following code:
--
-- @
--   \{\-\# LANGUAGE DataKinds \#\-\}
--   \{\-\# LANGUAGE TemplateHaskell \#\-\}
--
--   $('createApiDec' \"FrontAPI\" \"dir\")
-- @
--
-- You can think of it as expanding to the following:
--
-- @
--   type FrontAPI = $('createApiType' \"dir\")
-- @
createApiDec
  :: String   -- ^ name of the api type synonym
  -> FilePath -- ^ directory name to read files from
  -> Q [Dec]
createApiDec apiName templateDir =
  pure <$> tySynD (mkName apiName) [] (createApiType templateDir)
