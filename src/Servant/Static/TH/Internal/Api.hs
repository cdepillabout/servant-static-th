{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Static.TH.Internal.Api where

import Data.Foldable (foldl1)
import Data.List.NonEmpty (NonEmpty)
import Language.Haskell.TH
       (Dec, Q, Type, appT, litT, mkName,
        runIO, strTyLit, tySynD)
import Language.Haskell.TH.Syntax (addDependentFile)
import Servant.API (Get, (:<|>), (:>))
import System.FilePath (takeFileName)

import Servant.Static.TH.Internal.FileTree
import Servant.Static.TH.Internal.Mime

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

createApiDec :: String -> FilePath -> Q [Dec]
createApiDec apiName templateDir =
  pure <$> tySynD (mkName apiName) [] (createApiType templateDir)
