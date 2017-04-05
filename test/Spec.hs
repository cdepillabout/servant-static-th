{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Exception (Exception, SomeException, catch)
import Data.Typeable (Typeable)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), assertFailure, testCase)

import Servant.Raw.TH

testDir :: FilePath
testDir = "test" </> "test-dir"

main :: IO ()
main = do
  createDirectoryIfMissing False (testDir </> "empty-dir")
  defaultMain tests

tests :: TestTree
tests = testGroup "tests" [helperFunctionTests]

helperFunctionTests :: TestTree
helperFunctionTests =
  testGroup
    "helper functions"
    [ getFileTypeTests
    , getFileTreeIgnoreEmptyTests
    ]

getFileTypeTests :: TestTree
getFileTypeTests =
  testGroup
    "getFileType"
    [ testCase "correctly checks files" $ do
        let helloHtmlPath = testDir </> "hello.html"
        fileType <- getFileType helloHtmlPath
        FileTypeFile helloHtmlPath @?= fileType
    , testCase "correctly checks directories" $ do
        fileType <- getFileType testDir
        FileTypeDir testDir @?= fileType
    , testCase "fails for anything else" $
        getFileType "somelongfilethatdoesntexist" @! anyException
    ]

getFileTreeIgnoreEmptyTests :: TestTree
getFileTreeIgnoreEmptyTests =
  testGroup
    "getFileTreeIgnoreEmpty"
    [ testCase "correctly gets file tree" $ do
        actualFileTree <- getFileTreeIgnoreEmpty testDir
        let expectedFileTree =
              [ FileTreeDir
                  (testDir </> "dir")
                  [ FileTreeFile
                      (testDir </> "dir" </> "inner-file.html")
                      "Inner File\n"
                  ]
              , FileTreeFile
                  (testDir </> "hello.html")
                  "Hello World\n"
              ]
        actualFileTree @?= expectedFileTree
    , testCase "fails on empty directory" $
        getFileTreeIgnoreEmpty (testDir </> "empty-dir") @! anyException
    ]

type Selector a = a -> Bool

anyException :: Selector SomeException
anyException _ = True

assertThrows :: (Exception e, Typeable e) => IO a -> Selector e -> IO ()
assertThrows ioAction selector = do
  didCatch <- catch (ioAction *> pure False) (pure . selector)
  case didCatch of
    False ->
      assertFailure "expecting an exception, but no exception occurred"
    True -> pure ()

(@!) :: (Exception e, Typeable e) => IO a -> Selector e -> IO ()
(@!) = assertThrows

infix 1 @!
