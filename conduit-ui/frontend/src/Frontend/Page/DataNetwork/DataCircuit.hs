{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}


module Frontend.Page.DataNetwork.DataCircuit
  (dataNetwork_dataCircuit_handle, dataNetwork_dataCircuit) where

import Common.WebSocketMessage
import Prelude

import Reflex.Dom.Core
import Control.Monad (forM_, void)
import Control.Monad.Fix (MonadFix)

import qualified Data.Text as T
import Data.String.Conversions (cs)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent (MVar, newMVar, putMVar, modifyMVar, modifyMVar_, readMVar, threadDelay)
import qualified Data.Tree as TR

exampleDataCircuits :: [DataCircuit]
exampleDataCircuits =
  [ def { dataCircuit_name = "fileLoadPlatform"
        , dataCircuit_desc = "文件加载平台" }
  , def { dataCircuit_name = "dwSchedulePlatform"
        , dataCircuit_desc = "数仓调度平台" }
  , def { dataCircuit_name = "filePushPlatform"
        , dataCircuit_desc = "文件下传平台" }
  , def { dataCircuit_name = "dataQueryPlatform"
        , dataCircuit_desc = "数据查询平台" }
  , def { dataCircuit_name = "externalDataPlatform"
        , dataCircuit_desc = "外部数据平台" }
  , def { dataCircuit_name = "logPullPlatform"
        , dataCircuit_desc = "日志抽取平台" }
  , def { dataCircuit_name = "streamingPlatform"
        , dataCircuit_desc = "流式计算平台" }
  , def { dataCircuit_name = "dataMonitorPlatform"
        , dataCircuit_desc = "数据监控平台" }
    ]
dataNetwork_dataCircuit_handle
  :: forall t m r.
     ( MonadHold t m, MonadFix m
     , MonadIO m, MonadIO (Performable m), PerformEvent t m)
  => MVar r -> Event t WSResponseMessage -> m (Event t [DataCircuit])
dataNetwork_dataCircuit_handle _ wsResponseEvt = do
  return (const [] <$> wsResponseEvt)

theadUI
  :: forall t m .
     (DomBuilder t m, PostBuild t m)
  => m ()
theadUI = do
  el "thead" $ el "tr" $ do
    elClass "th" "" $ checkbox False def
    elClass "th" "" $ text "名称"
    elClass "th" "" $ text "描述"
    elClass "th" "" $ text "子数据电路"    
    elClass "th" "" $ text "数据导管"
--    elClass "th" "" $ text "部件组合器"

    {--
    elClass "th" "" $ text "配置数据模式"    
    elClass "th" "" $ text "请求数据模式"
    elClass "th" "" $ text "响应数据模式"
    --}

tbodyUI
  :: forall t m .
     (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
   => m ()
tbodyUI = do
  el "tbody" $ do
    elClass "tr" "warning" $ do
      el "td" $ elClass "i" "notched circle loading icon" blank
      -- name
      el "td" $ divClass "ui mini input" $ inputElement def
      -- description
      el "td" $ divClass "ui mini input" $ inputElement def
      el "td" $ divClass "ui mini input" $ inputElement def
      el "td" $ divClass "ui mini input" $ inputElement def      

  return ()

tfootUI
  :: forall t m .
     (DomBuilder t m, PostBuild t m)
   => m ()
tfootUI = do
  el "tfoot" $ do
    blank

dataNetwork_dataCircuit
  :: forall t m .
     (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Event t [DataCircuit]
  -> m (Event t [WSRequestMessage])
dataNetwork_dataCircuit wsEvt = do
  divClass "ui segment basic" $
    divClass "ui grid" $ divClass "eight wide column" $ divClass "ui message" $ do
      elClass "h2" "ui header" $ do
        text "数据电路"
      elClass "ul" "list" $ do
        el "li" $ elClass "h4" "ui header" $ text "由数据导管连接而成"
        el "li" $ elClass "h4" "ui header" $ text "可包含子数据电路"
        el "li" $ elClass "h4" "ui header" $ text "按需构建业务系统"
        

  divClass "ui segment basic" $ do
    elClass "table" "ui table collapsing" $ do
      theadUI
      tbodyUI
      tfootUI
  
  return never
