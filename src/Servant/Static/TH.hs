module Servant.Static.TH
  ( -- * Create API
    createApiType
  , createApiDec
    -- * Create Server
  , createServerExp
  , createServerDec
    -- * Create Both API and Server
  , createApiAndServerDecs
    -- * Mime Types
    -- | If you need additional MIME types supported, feel free to create an
    -- <https://github.com/cdepillabout/servant-static-th/issues issue> or
    -- <https://github.com/cdepillabout/servant-static-th/pulls PR>.
  , CSS
  , GIF
  , HTML
  , Html
  , JPEG
  , JS
  , PNG
  , SVG
  , TXT
    -- * Easy-To-Use Names and Paths
    -- | The functions in this section pick defaults for the api name and the
    -- server function name. This makes it easy to use.
    -- ** Paths and Names
  , frontEndTemplateDir
  , frontEndApiName
  , frontEndServerName
    -- ** API
  , createApiFrontEndType
  , createApiFrontEndDec
    -- ** Server
  , createServerFrontEndExp
  , createServerFrontEndDec
    -- ** Server and API
  , createApiAndServerFrontEndDecs
  ) where

import Language.Haskell.TH (Dec, Exp, Q, Type, mkName, tySynD)
import System.FilePath ((</>))
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html (Html)

import Servant.Static.TH.Internal
       (CSS, GIF, JPEG, JS, PNG, SVG, TXT, createApiDec, createApiType,
        createServerDec, createServerExp)

------------------------------------
-- Hard-coded Frontend file paths --
------------------------------------

-- | This is the directory @frontend/dist@.
frontEndTemplateDir :: FilePath
frontEndTemplateDir = "frontend" </> "dist"

-- | This is the 'String' @FrontEnd@.
frontEndApiName :: String
frontEndApiName = "FrontEnd"

-- | This is the 'String' @frontEndServer@.
frontEndServerName :: String
frontEndServerName = "frontEndServer"

---------
-- Api --
---------

createApiFrontEndType :: Q Type
createApiFrontEndType = createApiType frontEndTemplateDir

createApiFrontEndDec :: Q [Dec]
createApiFrontEndDec =
  pure <$> tySynD (mkName "FrontEnd") [] createApiFrontEndType

------------
-- Server --
------------

createServerFrontEndExp :: Q Exp
createServerFrontEndExp = createServerExp frontEndTemplateDir

createServerFrontEndDec :: Q [Dec]
createServerFrontEndDec =
  createServerDec frontEndApiName frontEndServerName frontEndTemplateDir

--------------------
-- Server and API --
--------------------

createApiAndServerDecs :: String -> String -> FilePath -> Q [Dec]
createApiAndServerDecs apiName serverName templateDir =
  let apiDecs = createApiDec apiName templateDir
      serverDecs = createServerDec apiName serverName templateDir
  in mappend <$> apiDecs <*> serverDecs

createApiAndServerFrontEndDecs :: Q [Dec]
createApiAndServerFrontEndDecs =
  createApiAndServerDecs frontEndApiName frontEndServerName frontEndTemplateDir
