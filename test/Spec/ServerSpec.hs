{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Spec.ServerSpec where

import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Servant.Server (serve)
import Test.Hspec.Wai (get, shouldRespondWith, with)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec, it)

import Servant.Raw.TH.Internal (createApiAndServerDecs)

import Spec.TestDirLocation (testDir)

$(createApiAndServerDecs "FrontEndApi" "testDirServer" testDir)

app :: Application
app = serve (Proxy @FrontEndApi) testDirServer

serverTestsIO :: IO TestTree
serverTestsIO =
  testSpec "server" $
    with (pure app) $ do
      it "works" $ do
        get "hello.html" `shouldRespondWith` "Hello World\n"
