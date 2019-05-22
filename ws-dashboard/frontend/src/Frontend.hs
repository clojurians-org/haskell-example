{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Frontend where

import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T
import Obelisk.Frontend

import Data.Default (def)
import qualified Data.Aeson as J
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString as B

import Control.Lens ((.~), (?~))
import Control.Monad (void)
import Reflex (
    Reflex(..), MonadSample(..), MonadHold(..)
  , PerformEvent(..), PostBuild(..), TriggerEvent(..), Event
  , never, fmapMaybe, sample, constDyn
  )
import Reflex.Dom.Core (
    DomBuilder
  --, Prerender(prerender)
  , el, text, elAttr, (=:), blank, divClass
  )
import Reflex.Dom.Prerender (Prerender, prerender)
import Reflex.Dom.WebSocket (webSocket, webSocketConfig_send, RawWebSocket (_webSocket_recv))
import Obelisk.Route (R)
import Obelisk.Route.Frontend (subRoute_)

import Data.Maybe (fromJust)
import Data.Map (Map, fromList)
import Data.Time (UTCTime, getCurrentTime)
import Reflex.Dom.Widget.ECharts (
    TimeLineChartConfig(..), ChartOptions(..), Series(..), SeriesLine
  , timeLineChart, series_smooth, series_name
  )
import Data.Time (parseTimeM, defaultTimeLocale)

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

apiStatWS :: forall t m js .
          ( DomBuilder t m, Prerender js m, PerformEvent t m, TriggerEvent t m, PostBuild t m)
          => m (Event t ApiStat)
apiStatWS = do
  wsRespEv <-
    prerender (return never) $ do
      ws <- webSocket "" $ def & webSocketConfig_send .~ (never :: Event t [Text])
      return (_webSocket_recv ws)
  return $ fmapMaybe (J.decode . fromStrict) wsRespEv

home :: ( DomBuilder t m, PostBuild t m, Prerender js m, PerformEvent t m, MonadHold t m
        , TriggerEvent t m
        ) => m ()
home = do
  pb <- getPostBuild
  let opts :: ChartOptions = undefined
  let mkTs s = fromJust $ parseTimeM True defaultTimeLocale "%FT%R" s :: UTCTime
  let chartData = 
          fromList [ ("success"
                        , ( def & series_smooth ?~ Left True & series_name ?~ "success"
                          , 15 :: Int
                          , [(mkTs "2018-01-01T05:03", 1.0 :: Double)] <$ pb))
                   , ("fail"
                        , ( def & series_smooth ?~ Left True & series_name ?~ "fail"
                          , 15 :: Int
                          , [(mkTs "2018-01-01T05:03", 2.0 :: Double)] <$ pb))
                     ]
  prerender blank $  void $ do
    timeLineChart $ TimeLineChartConfig (600, 400) (constDyn opts) chartData

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = pageHeader
  , _frontend_body = do
      el "main" $ divClass "" $ subRoute_ $ \case
        FrontendRoute_Ping -> text "pong"
        FrontendRoute_Home -> home
  }
