{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Static.TH.Internal where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (foldl1)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Proxy (Proxy)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Typeable (Typeable)
import Language.Haskell.TH
       (Dec, Exp, Q, Type, appE, appT, clause, conT, funD, litT, mkName,
        normalB, runIO, sigD, strTyLit, tySynD)
import Language.Haskell.TH.Syntax (addDependentFile)
import Network.HTTP.Media (MediaType, (//))
import Servant.HTML.Blaze (HTML)
import Servant.API
       (Accept(contentType), Get, MimeRender(mimeRender), (:<|>)((:<|>)),
        (:>))
import Servant.Server (ServerT)
import System.Directory
       (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), takeExtension, takeFileName)
import Text.Blaze.Html (Html, preEscapedToHtml)

----------------
-- Mime Types --
----------------

data MimeTypeInfo = MimeTypeInfo
  { mimeTypeInfoContentType :: Q Type
  , mimeTypeInfoRespType :: Q Type
  , mimeTypeInfoToExpression :: ByteString -> Q Exp
  }

extensionMimeTypeMap :: Map String MimeTypeInfo
extensionMimeTypeMap =
  [ ("html", MimeTypeInfo [t|HTML|] [t|Html|] htmlToExpression)
  , ("js", MimeTypeInfo [t|JS|] [t|ByteString|] byteStringToExpression)
  ]

byteStringToExpression :: ByteString -> Q Exp
byteStringToExpression byteString =
  let word8List = ByteString.unpack byteString
  in [e|pure $ ByteString.pack word8List|]

htmlToExpression :: ByteString -> Q Exp
htmlToExpression byteString =
  let fileContentsString = unpack $ decodeUtf8With lenientDecode byteString
  in [e|pure $ (preEscapedToHtml :: String -> Html) fileContentsString|]

-- | Remove a leading period from a string.
--
-- >>> removeLeadingPeriod ".jpg"
-- "jpg"
--
-- Just return the 'String' if it doesn't start with a period:
--
-- >>> removeLeadingPeriod "hello"
-- "hello"
--
-- Return an empty string if the only character in the string is a period:
--
-- >>> removeLeadingPeriod "."
-- ""
--
-- Remove at most one period:
--
-- >>> removeLeadingPeriod "..bye"
-- ".bye"
removeLeadingPeriod :: String -> String
removeLeadingPeriod ('.':chars) = chars
removeLeadingPeriod string = string

extensionToMimeTypeInfoEx :: FilePath -> Q MimeTypeInfo
extensionToMimeTypeInfoEx file =
  case extensionToMimeTypeInfo file of
    Just mimeTypeInfo -> pure mimeTypeInfo
    Nothing ->
      let extension = getExtension file
      in fail $
        "Unknown extension type \"" <> extension <> "\".  Please report as bug."

extensionToMimeTypeInfo :: FilePath -> Maybe MimeTypeInfo
extensionToMimeTypeInfo file =
  Map.lookup
    (removeLeadingPeriod $ takeExtension file)
    extensionMimeTypeMap

-- | Return an extension for a 'FilePath'.  Just like 'takeExtension', but
-- doesn't return the leading period.
--
-- >>> getExtension "/some/file.html"
-- "html"
--
-- Empty string is returned for files with no extension:
--
-- >>> getExtension "file"
-- ""
getExtension :: FilePath -> FilePath
getExtension = removeLeadingPeriod . takeExtension

data JS deriving Typeable

instance Accept JS where
  contentType :: Proxy JS -> MediaType
  contentType _ = "application" // "javascript"

instance MimeRender JS ByteString where
  mimeRender :: Proxy JS -> ByteString -> LByteString.ByteString
  mimeRender _ = LByteString.fromStrict

------------------------------------
-- Hard-coded Frontend file paths --
------------------------------------

frontEndTemplateDir :: FilePath
frontEndTemplateDir = "frontend" </> "dist"

frontEndApiName :: String
frontEndApiName = "FrontEnd"

frontEndServerName :: String
frontEndServerName = "frontEndServer"

----------------------------------------
-- Helper functions for reading files --
----------------------------------------

data FileTree
  = FileTreeFile FilePath ByteString
  | FileTreeDir FilePath (NonEmpty FileTree)
  deriving (Eq, Read, Show)

data FileType
  = FileTypeFile FilePath
  | FileTypeDir FilePath
  deriving (Eq, Read, Show)

getFileType :: FilePath -> IO FileType
getFileType path = do
  isFile <- doesFileExist path
  isDir <- doesDirectoryExist path
  case (isFile, isDir) of
    (True, _) -> pure $ FileTypeFile path
    (_, True) -> pure $ FileTypeDir path
    _ ->
      fail $
        "getFileType: Could not determine the type of file \"" <> path <> "\""

fileTypeToFileTree :: FileType -> IO (Maybe FileTree)
fileTypeToFileTree (FileTypeFile filePath) =
  Just . FileTreeFile filePath <$> ByteString.readFile filePath
fileTypeToFileTree (FileTypeDir dir) = do
  fileTrees <- getFileTree dir
  pure $
    case fileTrees of
      [] -> Nothing
      (ft:fts) -> Just . FileTreeDir dir $ ft :| fts

getFileTree :: FilePath -> IO [FileTree]
getFileTree templateDir = do
  filePaths <- sort <$> listDirectory templateDir
  let fullFilePaths = fmap (templateDir </>) filePaths
  fileTypes <- traverse getFileType fullFilePaths
  fileTreesWithMaybe <- traverse fileTypeToFileTree fileTypes
  pure $ catMaybes fileTreesWithMaybe

getFileTreeIgnoreEmpty :: FilePath -> IO (NonEmpty FileTree)
getFileTreeIgnoreEmpty templateDir = do
  fileTrees <- getFileTree templateDir
  case fileTrees of
    [] ->
      fail $
        "getFileTreeIgnoreEmpty: Top level template directory is empty: \"" <>
        templateDir <> "\""
    (ft:fts) -> pure $ ft :| fts

---------
-- Api --
---------

fileTreeToApiType :: FileTree -> Q Type
fileTreeToApiType (FileTreeFile filePath _) = do
  addDependentFile filePath
  MimeTypeInfo mimeT respT _ <- extensionToMimeTypeInfoEx filePath
  let fileNameLitT = litT $ strTyLit $ takeFileName filePath
  [t|$(fileNameLitT) :> Get '[$(mimeT)] $(respT)|]
fileTreeToApiType (FileTreeDir filePath fileTrees) =
  let fileNameLitT = litT $ strTyLit $ takeFileName filePath
  in [t|$(fileNameLitT) :> $(combineWithServantOrT nonEmptyApiTypesQ)|]
  where
    nonEmptyApiTypesQ :: NonEmpty (Q Type)
    nonEmptyApiTypesQ = fmap fileTreeToApiType fileTrees

combineWithServantOrT :: NonEmpty (Q Type) -> Q Type
combineWithServantOrT = foldl1 $ combineWithType [t|(:<|>)|]

combineWithType :: Q Type -> Q Type -> Q Type -> Q Type
combineWithType combiningType = appT . appT combiningType

createApiType :: FilePath -> Q Type
createApiType templateDir = do
  fileTree <- runIO $ getFileTreeIgnoreEmpty templateDir
  combineWithServantOrT $ fmap fileTreeToApiType fileTree

createApiFrontEndType :: Q Type
createApiFrontEndType = createApiType frontEndTemplateDir

createApiDec :: String -> FilePath -> Q [Dec]
createApiDec apiName templateDir =
  pure <$> tySynD (mkName apiName) [] (createApiType templateDir)

createApiFrontEndDec :: Q [Dec]
createApiFrontEndDec =
  pure <$> tySynD (mkName "FrontEnd") [] createApiFrontEndType

------------
-- Server --
------------

combineWithExp :: Q Exp -> Q Exp -> Q Exp -> Q Exp
combineWithExp combiningExp = appE . appE combiningExp

combineWithServantOr :: NonEmpty (Q Exp) -> Q Exp
combineWithServantOr = foldl1 $ combineWithExp [e|(:<|>)|]

fileTreeToServer :: FileTree -> Q Exp
fileTreeToServer (FileTreeFile filePath fileContents) = do
  addDependentFile filePath
  MimeTypeInfo _ _ contentToExp <- extensionToMimeTypeInfoEx filePath
  contentToExp fileContents
fileTreeToServer (FileTreeDir _ fileTrees) =
  combineWithServantOr $ fmap fileTreeToServer fileTrees

createServerExp :: FilePath -> Q Exp
createServerExp templateDir = do
  fileTree <- runIO $ getFileTreeIgnoreEmpty templateDir
  combineWithServantOr $ fmap fileTreeToServer fileTree

createServerFrontEndExp :: Q Exp
createServerFrontEndExp = createServerExp frontEndTemplateDir

createServerDec :: String -> String -> FilePath -> Q [Dec]
createServerDec apiName serverName templateDir =
  let funcName = mkName serverName
      sigTypeQ =
          [t|forall m. Applicative m => ServerT $(conT (mkName apiName)) m|]
      signatureQ = sigD funcName sigTypeQ
      clauses = [clause [] (normalB (createServerExp templateDir)) []]
      funcQ = funD funcName clauses
  in sequence [signatureQ, funcQ]

createServerFrontEndDec :: Q [Dec]
createServerFrontEndDec =
  createServerDec frontEndApiName frontEndServerName frontEndTemplateDir

--------------------
-- Server and API --
--------------------

createApiAndServerDecs :: String -> String -> FilePath -> Q [Dec]
createApiAndServerDecs apiName serverName templateDir =
  let apiDecs = createApiDec apiName templateDir
      serverDecs = createServerDec apiName serverName templateDir
  in mappend <$> apiDecs <*> serverDecs

createApiAndServerFrontEndDecs :: Q [Dec]
createApiAndServerFrontEndDecs =
  createApiAndServerDecs frontEndApiName frontEndServerName frontEndTemplateDir
