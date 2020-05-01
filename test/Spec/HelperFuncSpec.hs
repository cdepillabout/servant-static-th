{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Spec.HelperFuncSpec where

import qualified Data.ByteString as ByteString
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Servant.Static.TH.Internal
       (FileTree(..), FileType(..), getFileTreeIgnoreEmpty, getFileType)

import Spec.TastyHelpers ((@!), anyException)
import Spec.TestDirLocation (testDir)
import Control.Monad.IO.Class (liftIO)

helperFuncTests :: TestTree
helperFuncTests =
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
        gzippedJS   <- liftIO . ByteString.readFile $ testDir </> "dir" </> "test-gzipped.js.gz"
        brotliJS    <- liftIO . ByteString.readFile $ testDir </> "dir" </> "test-brotli.js.br"
        gzippedHTML <- liftIO . ByteString.readFile $ testDir </> "hello-compressed.html.gz"
        let expectedFileTree =
              [ FileTreeDir
                  (testDir </> "dir")
                  [ FileTreeFile
                      (testDir </> "dir" </> "inner-file.html")
                      "Inner File\n"
                  , FileTreeFile
                      (testDir </> "dir" </> "test-brotli.js.br")
                      brotliJS
                  , FileTreeFile
                      (testDir </> "dir" </> "test-gzipped.js.gz")
                      gzippedJS
                  , FileTreeFile
                      (testDir </> "dir" </> "test.js")
                      "console.log(\"hello world\");\n"
                  ]
              , FileTreeFile
                  (testDir </> "hello-compressed.html.gz")
                  gzippedHTML
              , FileTreeFile
                  (testDir </> "hello.html")
                  "Hello World\n"
              ]
        actualFileTree @?= expectedFileTree
    , testCase "fails on empty directory" $
        getFileTreeIgnoreEmpty (testDir </> "empty-dir") @! anyException
    ]
