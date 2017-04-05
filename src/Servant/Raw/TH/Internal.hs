{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Servant.Raw.TH.Internal where

import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Foldable (foldl1)
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
  | FileTreeDirectory FilePath (NonEmpty FileTree)
  deriving (Eq, Read, Show)

data FileType
  = FileTypeFile FilePath
  | FileTypeDirectory FilePath
  deriving (Eq, Read, Show)

getFileType :: FilePath -> Q FileType
getFileType path = do
  isFile <- runIO $ doesFileExist path
  isDir <- runIO $ doesDirectoryExist path
  case (isFile, isDir) of
    (True, _) -> pure $ FileTypeFile path
    (_, True) -> pure $ FileTypeDirectory path
    _ ->
      fail $
        "getFileType: Could not determine the type of file \"" <> path <> "\""

fileTypeToFileTree :: FileType -> Q (Maybe FileTree)
fileTypeToFileTree (FileTypeFile filePath) = do
  addDependentFile filePath
  Just . FileTreeFile filePath <$> runIO (ByteString.readFile filePath)
fileTypeToFileTree (FileTypeDirectory dir) = do
  fileTrees <- getFileTree dir
  pure $
    case fileTrees of
      [] -> Nothing
      (ft:fts) -> Just $ FileTreeDirectory dir $ ft :| fts

getFileTree :: FilePath -> Q [FileTree]
getFileTree templateDir = do
  filePaths <- runIO $ listDirectory templateDir
  let fullFilePaths = fmap (templateDir </>) filePaths
  fileTypes <- traverse getFileType fullFilePaths
  fileTreesWithMaybe <- traverse fileTypeToFileTree fileTypes
  pure $ catMaybes fileTreesWithMaybe

getFileTreeIgnoreEmpty :: FilePath -> Q (NonEmpty FileTree)
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
fileTreeToApiType (FileTreeFile filePath _) =
  servantNamedApiT (takeFileName filePath) [t|Get '[HTML] Html|]
fileTreeToApiType (FileTreeDirectory filePath fileTrees) =
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
  fileTree <- getFileTreeIgnoreEmpty templateDir
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
fileTreeToServer (FileTreeFile _ fileContents) =
  let fileContentsString = unpack $ decodeUtf8With lenientDecode fileContents
      fileContentsStringL = litE $ stringL fileContentsString
  in appE [e|pure . (preEscapedToHtml :: String -> Html)|] fileContentsStringL
fileTreeToServer (FileTreeDirectory _ fileTrees) =
  combineWithServantOr $ fmap fileTreeToServer fileTrees

createServerExp :: FilePath -> Q Exp
createServerExp templateDir = do
  fileTree <- getFileTreeIgnoreEmpty templateDir
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

createDecs :: String -> String -> FilePath -> Q [Dec]
createDecs apiName serverName templateDir =
  let apiDecs = createApiDec apiName templateDir
      serverDecs = createServerDec apiName serverName templateDir
  in mappend <$> apiDecs <*> serverDecs

createFrontEndDecs :: Q [Dec]
createFrontEndDecs =
  createDecs frontEndApiName frontEndServerName frontEndTemplateDir
