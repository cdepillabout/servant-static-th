{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [fooTests]

fooTests :: TestTree
fooTests = testGroup "foo" [fooTest]

fooTest :: TestTree
fooTest = testCase "works correctly" $ 1 @?= let expected = 1 in expected
