module Main where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Tasty (TestTree, defaultMain, testGroup)

import Spec.ApiSpec (apiTests)
import Spec.HelperFuncSpec (helperFuncTests)
import Spec.TestDirLocation (testDir)

main :: IO ()
main = do
  createDirectoryIfMissing False (testDir </> "empty-dir")
  defaultMain tests

tests :: TestTree
tests = testGroup "tests" [helperFuncTests, apiTests]
