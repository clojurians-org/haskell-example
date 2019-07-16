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

exampleCode :: String
exampleCode = unlines [
          "do"
        , "  let sql = [str|select"
        , "                |  id, name, description, 'type'"
        , "                |, state, timeliness, params, result_plugin_type"
        , "                |, vendor_id, server_id, success_code"
        , "                |from tb_interface"
        , "                |] :: B.ByteString"
        , "  let pgSettings = H.settings \"10.132.37.200\" 5432 \"monitor\" \"monitor\" \"monitor\""
        , "  let (curName, cursorSize, chanSize) = (\"larluo\", 200, 1000)"
        , "  let textColumn = HD.column HD.text"
        , "  let mkRow = (,,,,,,,,,,)"
        , "                <$> fmap (#id :=) textColumn"
        , "                <*> fmap (#name :=) textColumn"
        , "                <*> fmap (#description :=) textColumn"
        , "                <*> fmap (#type :=) textColumn"
        , "                <*> fmap (#state :=) textColumn"
        , "                <*> fmap (#timeliness :=) textColumn"
        , "                <*> fmap (#params :=) textColumn"
        , "                <*> fmap (#result_plugin_type :=) textColumn"
        , "                <*> fmap (#vendor_id :=) textColumn"
        , "                <*> fmap (#server_id :=) textColumn"
        , "                <*> fmap (#success_code :=) textColumn"
        , "  Right connection <- liftIO $ H.acquire pgSettings"
        , "  runResourceT $ do"
        , "    chan <- pgToChan connection sql curName cursorSize chanSize mkRow"
        , "    runConduit $"
        , "      (sourceTBMChan chan"
        , "            .| C.concat"
        , "            .| C.take 2"
        , "            .| C.mapM_ (liftIO . print))"
        ]

page :: forall t js m. ( DomBuilder t m, Prerender js m
        , PerformEvent t m, TriggerEvent t m, PostBuild t m) => m ()
page = do
  divClass "ui segment basic" $ 
    divClass "ui form" $ do
      myInput <- divClass "ui field" $ do
        textAreaElement $ def & initialAttributes .~ ("rows" =: "20")
                              & textAreaElementConfig_initialValue .~ (cs exampleCode)
                              
      runEvt :: (Event t ()) <- divClass "ui field" $ do
        domEvent Click . fst <$> elClass' "button" "ui button blue" (text "RUN")

      wsEvt :: (Event t B.ByteString) <- prerender (return never) $ do
        ws <- webSocket "ws://10.132.37.200:4444/wsConduit" $
--                def & webSocketConfig_send .~ (never :: Event t [T.Text])
                def & webSocketConfig_send .~ (fmap (:[]) $ tag (current . value $ myInput) runEvt)
        return (_webSocket_recv ws)

      divClass "ui field" $
        textAreaElement $ def & initialAttributes .~ ("rows" =: "10")
                              & textAreaElementConfig_initialValue .~ ""
                              & textAreaElementConfig_setValue .~ (fmap cs wsEvt)
  return ()
  
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = htmlHeader
  , _frontend_body = do
      page
  }
