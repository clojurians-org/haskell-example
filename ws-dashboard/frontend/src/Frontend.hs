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

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Fix (MonadFix)
import Language.Javascript.JSaddle (MonadJSM)
import Reflex (
    Reflex(..), MonadSample(..), MonadHold(..)
  , PerformEvent(..), PostBuild(..), TriggerEvent(..), Event
  , never, fmapMaybe, sample, constDyn
  )
import Reflex.Dom.Core (
    DomBuilder
  , GhcjsDomSpace, DomBuilderSpace
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
    TimeLineChartConfig(..), LineChartConfig(..)
  , ChartOptions(..), Series(..), SeriesLine(..), Chart(..)
  , Data(DataInt), AxisType(..), Title(..)
  , timeLineChart, lineChart
  , series_smooth, series_name, series_areaStyle
  , chartOptions_title, title_text
  , chartOptions_xAxis, chartOptions_yAxis
  , axis_data, axis_type, axis_min, axis_max
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

myChart
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , TriggerEvent t m
     , MonadFix m
     , MonadIO (Performable m)
     , MonadJSM m
     , MonadJSM (Performable m)
     )
     => m (Chart t)
myChart = do
  pb <- getPostBuild
  let opts :: ChartOptions =
        def & chartOptions_title ?~ (def & title_text ?~ "My-Chart")
            & chartOptions_xAxis .~ (def & axis_type ?~ AxisType_Time) : []
            & chartOptions_yAxis .~ (def
                & axis_type ?~ AxisType_Value
                & axis_min ?~ Left 0
                & axis_max ?~ Left 101
              ) : []

  let mkTs s = fromJust $ parseTimeM True defaultTimeLocale "%FT%R" s :: UTCTime
  let chartData = 
          fromList [ ("success"
                        , ( def & series_smooth ?~ Left True & series_name ?~ "success" :: Series SeriesLine
                          , 15 :: Int
                          , [(mkTs "2018-01-01T05:03", 1.0 :: Double)] <$ pb))
                   , ("fail"
                        , ( def & series_smooth ?~ Left True & series_name ?~ "fail" :: Series SeriesLine
                          , 15 :: Int
                          , [(mkTs "2018-01-01T05:03", 2.0 :: Double)] <$ pb))
                     ]
  timeLineChart $ TimeLineChartConfig (600, 400) (constDyn opts) chartData

basicLineChart
  :: ( PostBuild t m
     , DomBuilder t m
     , PerformEvent t m
     , MonadHold t m
     , GhcjsDomSpace ~ DomBuilderSpace m
     , TriggerEvent t m
     , MonadFix m
     , MonadIO (Performable m)
     , MonadJSM m
     , MonadJSM (Performable m)
     )
  => m (Chart t)
basicLineChart = do
  let xAxisData = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
  let yAxisData = fromList $ zip xAxisData $ map DataInt $ reverse [820, 932, 901, 934, 1290, 1330, 1320]
  let yAxisData2 = fromList $ zip xAxisData $ map DataInt $ [820, 932, 901, 934, 1290, 1330, 1320]
  let basicLineChartOpts :: ChartOptions = def
        & chartOptions_yAxis .~ (def
          & axis_type ?~ AxisType_Value
          ) : []
        & chartOptions_xAxis .~ (def
          & axis_type ?~ AxisType_Category
          & axis_data ?~ (zip xAxisData $ repeat Nothing)
          ) : []
  let dd2Series = def
        & series_smooth ?~ Left True
        & series_areaStyle ?~ def
  let chartDataDyn = ((0::Int)  =: (def, (constDyn yAxisData), (constDyn xAxisData)))
                   <> (1 =: (dd2Series, (constDyn yAxisData2), (constDyn xAxisData)))
  lineChart (LineChartConfig (600, 400)
              (constDyn basicLineChartOpts)
              chartDataDyn
            )

home :: ( DomBuilder t m, PostBuild t m, Prerender js m, PerformEvent t m, MonadHold t m
        , TriggerEvent t m
        ) => m ()
home = do
  prerender blank $ elAttr "div" ("style" =: "display: flex; flex-wrap: wrap") $ do
    text "hello world-1"
    void $ elAttr "div" ("style" =: "padding: 50px;") $ myChart
    void $ elAttr "div" ("style" =: "padding: 50px;") $ basicLineChart
    text "hello world-3"
  return ()

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = pageHeader
  , _frontend_body = do
      el "main" $ divClass "" $ subRoute_ $ \case
        FrontendRoute_Ping -> text "pong"
        FrontendRoute_Home -> home
  }
