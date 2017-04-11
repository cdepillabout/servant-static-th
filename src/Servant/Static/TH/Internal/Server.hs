{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Servant.Static.TH.Internal.Server where

import Data.Foldable (foldl1)
import Data.List.NonEmpty (NonEmpty)
import Language.Haskell.TH
       (Dec, Exp, Q, appE, clause, conT, funD, mkName, normalB,
        runIO, sigD)
import Language.Haskell.TH.Syntax (addDependentFile)
import Servant.API ((:<|>)((:<|>)))
import Servant.Server (ServerT)

import Servant.Static.TH.Internal.FileTree
import Servant.Static.TH.Internal.Mime

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

createServerDec :: String -> String -> FilePath -> Q [Dec]
createServerDec apiName serverName templateDir =
  let funcName = mkName serverName
      sigTypeQ =
          [t|forall m. Applicative m => ServerT $(conT (mkName apiName)) m|]
      signatureQ = sigD funcName sigTypeQ
      clauses = [clause [] (normalB (createServerExp templateDir)) []]
      funcQ = funD funcName clauses
  in sequence [signatureQ, funcQ]
