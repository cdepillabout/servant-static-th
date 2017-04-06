{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE OverloadedLists #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Spec.ApiSpec where

import Data.Type.Equality ((:~:)(Refl))
import Servant.HTML.Blaze (HTML)
import Servant.API ((:<|>), (:>), Get)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Text.Blaze.Html (Html)

import Servant.Raw.TH.Internal (createApiDec)

import Spec.TestDirLocation (testDir)

$(createApiDec "FrontEndApi" testDir)

type ExpectedFrontEndApi =
    ("dir" :> "inner-file.html" :> Get '[HTML] Html)
  :<|>
    ("hello.html" :> Get '[HTML] Html)

checkFrontEndApiType :: ExpectedFrontEndApi :~: FrontEndApi
checkFrontEndApiType = Refl

createdCorrectlyTest :: TestTree
createdCorrectlyTest =
  testCase "created correctly" $ checkFrontEndApiType @?= Refl

apiTests :: TestTree
apiTests = testGroup "api" [createdCorrectlyTest]
