module Servant.Static.TH where

import Language.Haskell.TH (Dec, Exp, Q, Type, mkName, tySynD)
import System.FilePath ((</>))

import Servant.Static.TH.Internal
       (createApiDec, createApiType, createServerDec, createServerExp)

------------------------------------
-- Hard-coded Frontend file paths --
------------------------------------

frontEndTemplateDir :: FilePath
frontEndTemplateDir = "frontend" </> "dist"

frontEndApiName :: String
frontEndApiName = "FrontEnd"

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
