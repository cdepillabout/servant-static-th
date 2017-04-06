{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Servant.Raw.TH.Internal where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
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
       (Dec, Exp, Q, Type, appE, appT, clause, conT, funD, litE, litT,
        mkName, normalB, promotedConsT, promotedNilT, runIO, sigD, stringL,
        strTyLit, tySynD)
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

extensionToMimeType :: FilePath -> (Q Type, Q Type)
extensionToMimeType file =
  case takeExtension file of
    ".html" -> ([t|HTML|], [t|Html|])
    ".js" -> ([t|JS|], [t|ByteString|])

data JS deriving Typeable

instance Accept JS where
  contentType :: Proxy JS -> MediaType
  contentType _ = "application" // "javascript"

instance MimeRender JS LByteString.ByteString where
  mimeRender :: Proxy JS -> LByteString.ByteString -> LByteString.ByteString
  mimeRender _ = id

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

singletonPromotedListT :: Q Type -> Q Type
singletonPromotedListT singleT =
  appT (appT promotedConsT singleT) promotedNilT

fileTreeToApiType :: FileTree -> Q Type
fileTreeToApiType (FileTreeFile filePath _) = do
  addDependentFile filePath
  let (mimeT, respT) = extensionToMimeType filePath
      contentTypeT = singletonPromotedListT mimeT
      respApiTypeT = appT (appT [t|Get|] contentTypeT) respT
  servantNamedApiT (takeFileName filePath) respApiTypeT
fileTreeToApiType (FileTreeDir filePath fileTrees) =
  servantNamedApiManyT (takeFileName filePath) nonEmptyApiTypesQ
  where
    nonEmptyApiTypesQ :: NonEmpty (Q Type)
    nonEmptyApiTypesQ = fmap fileTreeToApiType fileTrees

servantNamedApiT :: String -> Q Type -> Q Type
servantNamedApiT name apiT =
  let nameT = appT [t|(:>)|] . litT $ strTyLit name
  in appT nameT apiT

servantNamedApiManyT :: String -> NonEmpty (Q Type) -> Q Type
servantNamedApiManyT name apiTs =
  servantNamedApiT name $ combineWithServantOrT apiTs

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
  let fileContentsString = unpack $ decodeUtf8With lenientDecode fileContents
      fileContentsStringL = litE $ stringL fileContentsString
  appE [e|pure . (preEscapedToHtml :: String -> Html)|] fileContentsStringL
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
