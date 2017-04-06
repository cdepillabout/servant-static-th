{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Servant.Raw.TH.Internal where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Foldable (foldl1)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Language.Haskell.TH
       (Dec, Exp, Q, Type, appE, appT, clause, conT, funD, litE, litT,
        mkName, normalB, runIO, sigD, stringL, strTyLit, tySynD)
import Language.Haskell.TH.Syntax (addDependentFile)
import Servant.HTML.Blaze (HTML)
import Servant.API ((:<|>)((:<|>)), (:>), Get)
import Servant.Server (ServerT)
import System.Directory
       (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), takeFileName)
import Text.Blaze.Html (Html, preEscapedToHtml)

frontEndTemplateDir :: FilePath
frontEndTemplateDir = "frontend" </> "dist"

frontEndApiName :: String
frontEndApiName = "FrontEnd"

frontEndServerName :: String
frontEndServerName = "frontEndServer"

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
  servantNamedApiT (takeFileName filePath) [t|Get '[HTML] Html|]
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
