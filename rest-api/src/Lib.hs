{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    (libMain
    ) where

import GHC.Generics (Generic)
import Data.Proxy (Proxy(..))
import Network.Wai.Handler.Warp (run)

import Data.Aeson
import Control.Lens ((?~), (.~), (&), makeLenses)
import Servant.API ((:<|>), (:>), Get, JSON, Summary)
import Servant.Server (Server, serve)
import Data.Swagger (Swagger, description, info, title, version)
import Servant.Swagger (toSwagger)
import Servant.Swagger.UI (SwaggerSchemaUI)
import Servant.Swagger.UI.ReDoc (redocSchemaUIServer)

libMain=putStrLn "hello world"

type MyAPI = "myAPI" :> Summary "my API" :> Get '[JSON] String
              
type DocReq = ("docs" :> "v1" :> SwaggerSchemaUI "index" "swagger.json")
type ApiReq = ("api" :> "v1" :> MyAPI)

docHandler = redocSchemaUIServer $ toSwagger (Proxy::Proxy MyAPI) & info.title .~ "My API"
apiHandler = redocSchemaUIServer $ toSwagger (Proxy::Proxy MyAPI) & info.title .~ "My API"
main = run 1111 $ serve (Proxy::Proxy (DocReq :<|> ApiReq)) (docHandler :<|> apiHandler)

{--
  run 1111 $ serve (Proxy::Proxy UserAPI1) (return "hello world")

--}
