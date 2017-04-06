{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Spec.ApiSpec where

import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Servant.Raw.TH.Internal
       (FileTree(..), FileType(..), getFileTreeIgnoreEmpty, getFileType)

import Spec.TestDirLocation (testDir)

