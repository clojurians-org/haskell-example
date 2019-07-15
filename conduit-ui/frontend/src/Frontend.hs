{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Frontend where

import Prelude
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core

import Control.Monad.IO.Class (MonadIO)

import Common.Api
import Common.Route
import Obelisk.Generated.Static

import Data.Default (def)
import Data.String.Conversions (cs)

import Reflex (never)

htmlHeader :: DomBuilder t m => m ()
htmlHeader = do
  elAttr "link" ( "rel" =: "stylesheet"
               <> "href" =: "https://cdn.jsdelivr.net/npm/semantic-ui@2.3.3/dist/semantic.min.css") blank

page :: forall t js m. ( DomBuilder t m, Prerender js m
        , PerformEvent t m, TriggerEvent t m, PostBuild t m) => m ()
page = do
  divClass "ui segment basic" $ 
    divClass "ui form" $ do
      divClass "ui field" $
        textAreaElement $ def & initialAttributes .~ ("rows" =: "10")
                              & textAreaElementConfig_initialValue .~ ""
                              
      runEvt <- divClass "ui field" $ do
        domEvent Click . fst <$> elClass' "button" "ui button blue" (text "RUN")

      wsEvt :: (Event t B.ByteString) <- prerender (return never) $ do
        ws <- webSocket "ws://10.132.37.200:4444/wsConduit" $
                def & webSocketConfig_send .~ (never :: Event t [T.Text])
--              def & webSocketConfig_send .~ ([""::B.ByteString] <$ runEvt)
        return (_webSocket_recv ws)

      divClass "ui field" $
        textAreaElement $ def & initialAttributes .~ ("rows" =: "10")
                              & textAreaElementConfig_initialValue .~ ""
                              & textAreaElementConfig_setValue .~ ("hello" <$ runEvt)
  return ()
  
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = htmlHeader
  , _frontend_body = do
      page
  }
