{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)
import Servant.Static.TH (createApiAndServerDecs)

-- 'createApiAndServerDecs' will use the files in the directory @example/example-dir@
-- to create two things.
--
-- Let's assume that the @example/example-dir@ directory looks like this:
--
-- @
--   $ tree example/example-dir/
--   example/example-dir/
--   ├── dir
--   │   ├── inner-file.html
--   │   └── example.js
--   └── hello.html
-- @
--
-- First, the following API definition will be created:
--
-- @
--   type FrontEndApi =
--       "dir" :>
--         ( "inner-file.html" :> Get '[HTML] Html :<|>
--           "example.js" :> Get '[JS] ByteString
--         ) :<|>
--       "hello.html" :> Get '[HTML] Html
-- @
--
-- Next, the following function will be created.  This function represents a
-- Servant server for the @FrontEndApi@.  It basically just returns the content
-- from the files in the @example/example-dir@ directory.  The contents from the files
-- is statically embedded in the @frontEndServer@ function at compile fime:
--
-- @
--   frontEndServer :: Applicative m => ServerT FrontEndApi m
--   frontEndServer = ...
-- @
--
-- This @frontEndServer@ function can be passed to Servant's 'serve' function
-- in order to create a WAI application.
--
-- If this WAI application is running, it is possible to use @curl@ to access
-- the server:
--
-- @
--   $ curl localhost:8080/hello.html
--   Hello World
--   $ curl localhost:8080/dir/inner-file.html
--   Inner File
--   $ curl localhost:8080/dir/example.js
--   console.log(\"hello world\");
-- @

$(createApiAndServerDecs "FrontEndApi" "frontEndServer" "example/example-dir")

app :: Application
app = serve (Proxy :: Proxy FrontEndApi) frontEndServer

main :: IO ()
main = run 8080 app
