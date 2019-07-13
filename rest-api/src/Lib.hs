{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib
    (libMain
    ) where

import GHC.Generics (Generic)
import Data.Proxy (Proxy(..))
import NeatInterpolation (text)

import Network.Wai.Handler.Warp (run)
import Data.ByteString (ByteString)
import Data.Aeson
import Data.Text (Text(..))
import Control.Lens ((?~), (.~), (&), makeLenses)
import Servant.API ((:<|>), (:>), Get, JSON, Summary, Capture', Description, QueryParam', Required, Optional)
import Servant.Server (Server, serve, Handler)
import Data.Swagger (Swagger, description, info, title, version)
import Servant.Swagger (toSwagger)
import FileEmbedLzma (embedRecursiveDir)
import Servant.Swagger.UI.Core (swaggerSchemaUIServerImpl)
import Servant.Swagger.UI (SwaggerSchemaUI, SwaggerSchemaUI')

libMain=putStrLn "hello world"

redocSchemaUIServer
    :: (Server api ~ Handler Swagger)
    => Swagger -> Server (SwaggerSchemaUI' dir api)
redocSchemaUIServer =
    swaggerSchemaUIServerImpl redocIndexTemplate redocFiles
  where
    redocFiles :: [(FilePath, ByteString)]
    redocFiles = $(embedRecursiveDir "redoc-dist")

    redocIndexTemplate :: Text
    redocIndexTemplate = [text|
<!doctype html>
<html lang="en">
  <head>
    <title>ReDoc</title>
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <style>
      body { margin: 0; padding: 0; }
    </style>
    <script>
        // Force Strict-URL Routing for assets relative paths
        (function onload() {
            if (!window.location.href.endsWith("/")) {
                window.location.href += "/";
            }
        }());
    </script>
  </head>
  <body>
    <redoc spec-url="../SERVANT_SWAGGER_UI_SCHEMA"></redoc>
    <script src="redoc.min.js"> </script>
  </body>
</html>
|]

type BasicReq = Get '[JSON] String
type DocReq = ("docs" :> "v1" :> SwaggerSchemaUI "index" "swagger.json")
type FirstCatEndPoint = "cat" :> Summary "First cat endpoint"
                              :> Capture' '[Description "Cat's name"] ":name" Text
                              :> QueryParam' '[Required, Description "Random number"] "num" Int
                              :> QueryParam' '[Optional, Description "Random text"] "text" Text
                              :> Get '[JSON] [String]
type ApiReq = ("api" :> "v1" :> (
                "myAPI1" :> Summary "my API 1" :> Get '[JSON] [String] :<|>
                "myAPI2" :> Summary "my API 2" :> Get '[JSON] [String])) :<|> FirstCatEndPoint


docHandler :: (Server api ~ Handler Swagger) => Server (SwaggerSchemaUI' dir api)
docHandler = redocSchemaUIServer $ toSwagger (Proxy::Proxy ApiReq) & info.title .~ "My API"

main = run 1111 $ serve (Proxy::Proxy DocReq) docHandler
-- main = run 1111 $ serve (Proxy::Proxy (DocReq :<|> ApiReq)) (docHandler :<|> apiHandler)

{--
  run 1111 $ serve (Proxy::Proxy UserAPI1) (return "hello world")
--}
