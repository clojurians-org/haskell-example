{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Backend.HttpServer (serveHTTP) where

import Common.Api
import Common.WebSocketMessage
import Prelude

import GHC.Int (Int64)
import GHC.Generics (Generic)
import Data.Proxy (Proxy(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.String.Conversions (cs)

import qualified Data.Text as T
import qualified Data.Aeson as J


import Snap.Core (Snap, liftSnap)
import Servant.Server (serveSnap)
import Servant.API

import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS

serveHTTP :: Snap ()
serveHTTP = serveSnap (Proxy::Proxy MyAPI) myAPI

type MyAPI =
  "api" :> ("ping" :> Get '[PlainText] T.Text
       :<|> "event" :> Capture "name" T.Text :> Post '[JSON] APIEventResponse)

myAPI = pong :<|> event
  where pong :: Snap T.Text
        pong = return "pong\n"
        event :: T.Text -> Snap APIEventResponse
        event name = do
          wsRet <- liftIO $ withSocketsDo $ do
            (host, port, path) <- askWSInfo
            WS.runClient (cs host) port (cs path) $ \conn -> do
              (WS.sendTextData conn . J.encode . HttpEventInvokeRequest) name
              _  {-- drop init --} <- WS.receiveData conn :: IO T.Text 
              WS.receiveData conn
              <* WS.sendClose conn ("Byte!" :: T.Text)
          return $ APIEventResponse wsRet 0

data APIEventRequest = APIEventRequest
  deriving (Generic, Show, Eq)
instance J.FromJSON APIEventRequest
data APIEventResponse = APIEventResponse {
    apiEventResponse_ret :: T.Text
  , apiEventResponse_cost :: Int64
  } deriving (Generic, Show, Eq)
instance J.ToJSON APIEventResponse
