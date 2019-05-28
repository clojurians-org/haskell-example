{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
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
  , never, fmapMaybe, sample, constDyn, ffor
  , tickLossyFromPostBuildTime, foldDyn
  )
import Reflex.Dom.Core (
    DomBuilder
  , GhcjsDomSpace, DomBuilderSpace, TickInfo
  --, Prerender(prerender)
  , el, text, elAttr, (=:), blank, divClass
  , rangeInput, rangeInputConfig_initialValue, rangeInputConfig_attributes
  , dyn, dynText, value, switchHold
  )
import Reflex.Dom.Prerender (Prerender, prerender)
import Reflex.Dom.WebSocket (webSocket, webSocketConfig_send, RawWebSocket (_webSocket_recv))
import Obelisk.Route (R)
import Obelisk.Route.Frontend (subRoute_)

import Data.Maybe (fromJust)
import Data.Either (fromRight)
import Data.Either.Combinators (rightToMaybe, mapLeft)
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
import Text.Heredoc (str)
import Data.String.Conv (toS)

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
          => m (Event t (Either String ApiStat))
apiStatWS = do
  wsRespEv <-
    prerender (return never) $ do
      ws <- webSocket "ws://10.132.37.200:3000/chat/eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJtb2RlIjoicncifQ.QKGnMJe41OFZcjz_qQSplmWAmVd_hmVjijKUNoJYpis" $
              def & webSocketConfig_send .~ (never :: Event t [Text])
      return (_webSocket_recv ws)
  return $ ffor wsRespEv $ \bs -> do
    let lbs = fromStrict bs
    mapLeft (const (toS lbs)) $ (J.eitherDecode lbs >>= J.eitherDecode . toS . _channelMsg_payload)

tickWithSpeedSelector
 :: ( PostBuild t m
    , DomBuilder t m
    , PerformEvent t m
    , MonadFix m
    , MonadHold t m
    , GhcjsDomSpace ~ DomBuilderSpace m
    , TriggerEvent t m
    , MonadIO (Performable m)
    )
  => m (Event t TickInfo)
tickWithSpeedSelector = do
  dyn ((\v -> tickLossyFromPostBuildTime (fromRational $ toRational v)) <$> (constDyn 1))
    >>= switchHold never
  
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
     => Event t [ApiStat] -> m (Chart t)
myChart ev  = do
  let opts :: ChartOptions =
        def & chartOptions_title ?~ (def & title_text ?~ "My-Chart")
            & chartOptions_xAxis .~ (def & axis_type ?~ AxisType_Time) : []
            & chartOptions_yAxis .~ (def & axis_type ?~ AxisType_Value) : []

  let chartData = 
          fromList [ ("success"
                        , ( def & series_smooth ?~ Left True & series_name ?~ "success" :: Series SeriesLine
                          , 15 :: Int
                          , ffor ev $ map (\(ApiStat ts succ fail) -> (ts, (fromIntegral succ))) ))
                   , ("fail"
                        , ( def & series_smooth ?~ Left True & series_name ?~ "fail" :: Series SeriesLine
                          , 15 :: Int
                          , ffor ev $ map (\(ApiStat ts succ fail) -> (ts, (fromIntegral fail))) )) ]
  timeLineChart $ TimeLineChartConfig (600, 400) (constDyn opts) chartData

mkTs :: String -> UTCTime
-- mkTs s = fromJust $ parseTimeM True defaultTimeLocale "%F %R" s
mkTs s = fromJust $ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M %Z" s

home :: ( DomBuilder t m, PostBuild t m, Prerender js m, PerformEvent t m, MonadHold t m
        , TriggerEvent t m
        ) => m ()
home = do
    -- pb <- getPostBuild

  prerender blank $ elAttr "div" ("style" =: "display: flex; flex-wrap: wrap") $ do
    void $ elAttr "div" ("style" =: "padding: 50px;") $ do
{--
      tickE <- tickWithSpeedSelector
      myChart ([ ApiStat (mkTs "2018-01-01 05:04 +0000") 1 5
               , ApiStat (mkTs "2018-01-01 05:05 +0000") 3 2
--               , ApiStat (mkTs "2018-01-01T05:06") 4.0 3.0
               ] <$ tickE)
--}
      wsE <- apiStatWS
      msg <- foldDyn (\m ms -> toS (show m)) "" wsE
      dynText ("MSG:" <> msg)

      let ev = fmapMaybe rightToMaybe wsE
      myChart (fmap (:[]) ev)
  return ()

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = pageHeader
  , _frontend_body = do
      el "main" $ divClass "" $ subRoute_ $ \case
        FrontendRoute_Ping -> text "pong"
        FrontendRoute_Home -> home
  }

repl :: IO ()
repl = do
  let apiJson = [str|{"ts":"2018-03-01 01:01 +0000","success_num":3,"fail_num":2}|]
  print (J.eitherDecode apiJson :: Either String ApiStat)
