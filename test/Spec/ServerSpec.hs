{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Spec.ServerSpec where

import qualified Data.ByteString.Lazy as ByteString
import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Servant.Server (serve)
import Test.Hspec.Wai ( ResponseMatcher(..)
                      , (<:>)
                      , get
                      , shouldRespondWith
                      , with
                      , MatchBody )
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)
import Test.Hspec (it)

import Servant.Static.TH (createApiAndServerDecs)

import Spec.TestDirLocation (testDir)
import Control.Monad.IO.Class (liftIO)
import System.FilePath ((</>))
import Data.ByteString (unpack)
import Test.Hspec.Wai.Matcher (bodyEquals)

$(createApiAndServerDecs "FrontEndApi" "testDirServer" testDir)

app :: Application
app = serve (Proxy @FrontEndApi) testDirServer

serverTestsIO :: IO TestTree
serverTestsIO =
  testSpec "server" $
    with (pure app) $ do
      it "hello.html responds correctly and is html" $
        let expectedResp =
              "Hello World\n"
                { matchHeaders = ["Content-Type" <:> "text/html;charset=utf-8"]
                }
        in get "hello.html" `shouldRespondWith` expectedResp
      it "hello-compressed.html responds correctly and is html" $ do
        gzippedHTML <- liftIO . ByteString.readFile $ testDir </> "hello-compressed.html.gz"
        let response = ResponseMatcher { matchHeaders = [ "Content-Encoding" <:> "gzip"
                                                        , "Content-Type"     <:> "text/html;charset=utf-8"
                                                        ]
                                       , matchBody    = bodyEquals gzippedHTML
                                       , matchStatus  = 200
                                       }
        get "hello-compressed.html" `shouldRespondWith` response
      it "dir/inner-file.html responds correctly" $
        get "dir/inner-file.html" `shouldRespondWith` "Inner File\n"
      it "dir/test-gzipped.js responds correctly with JS script" $ do
        gzippedJS <- liftIO . ByteString.readFile $ testDir </> "dir" </> "test-gzipped.js.gz"
        let response = ResponseMatcher { matchHeaders = ["Content-Encoding" <:> "gzip"]
                                       , matchBody    = bodyEquals gzippedJS
                                       , matchStatus  = 200
                                       }
        get "dir/test-gzipped.js" `shouldRespondWith` response
      it "dir/test-brotli.js responds correctly with JS script" $ do
        brotliJS <- liftIO . ByteString.readFile $ testDir </> "dir" </> "test-brotli.js.br"
        let response = ResponseMatcher { matchHeaders = ["Content-Encoding" <:> "br"]
                                       , matchBody    = bodyEquals brotliJS
                                       , matchStatus  = 200
                                       }
        get "dir/test-brotli.js" `shouldRespondWith` response
      it "non existing file gives 404" $
        get "somefilethatdoesntexist.html" `shouldRespondWith` 404
