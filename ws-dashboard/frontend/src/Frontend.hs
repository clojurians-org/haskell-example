{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
module Frontend where

import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T
import Obelisk.Frontend

import Data.Default (def)
import qualified Data.Aeson as J
import Data.ByteString.Lazy (fromStrict)

import Control.Lens ((.~))
import Reflex (
    Event, never
  )
import Reflex.Dom.Core (
    DomBuilder, Prerender(prerender)
  , el, text, elAttr, (=:), blank, divClass
  )
import Reflex.Dom.WebSocket (webSocket, webSocketConfig_send, RawWebSocket (_webSocket_recv))
import Obelisk.Route (R)
import Obelisk.Route.Frontend (subRoute_)

import Common.Api
import Common.Route
import Common.Types
import Obelisk.Generated.Static


styleSheet :: DomBuilder t m => Text -> m ()
styleSheet myLink = elAttr "link" attrs blank
  where attrs = "rel" =: "stylesheet"
             <> "type" =: "text/css"
             <> "href" =: myLink

pageHeader :: DomBuilder t m => m ()
pageHeader = do
  el "title" $ text "websocket report"
  elAttr "meta" ("charset" =: "utf-8") blank
  elAttr "script" ("type" =: "text/javascript" <> "src" =: static @"echarts.min.js") blank

apiStatWS :: (DomBuilder t m, Prerender js t m) => m (Event t ApiStat)
apiStatWS = do
  wsRespEv <- prerender (return never) $ do
    ws <- webSocket "" $ def & webSocketConfig_send .~ (never :: Event t [Text])
    return (_webSocket_recv ws)
  return $ fmapMaybe (J.decode . fromStrict) wsRespEv

home :: DomBuilder t m => m ()
home = undefined

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = pageHeader
  , _frontend_body = do
      el "main" $ divClass "" $ subRoute_ $ \case
        FrontendRoute_Ping -> text "pong"
        FrontendRoute_Home -> home
  }
