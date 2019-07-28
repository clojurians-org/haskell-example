{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}


module Frontend.Page.DataNetwork.EffectEngine
  (dataNetwork_effectEngine_handle, dataNetwork_effectEngine) where

import Common.WebSocketMessage
import Common.Types.DataNetwork
import Prelude

import Reflex.Dom.Core
import Control.Monad (forM_, void)
import Control.Monad.Fix (MonadFix)

import qualified Data.Text as T
import Data.String.Conversions (cs)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent (MVar, newMVar, putMVar, modifyMVar, modifyMVar_, readMVar, threadDelay)
import qualified Data.Tree as TR

dataNetwork_effectEngine_handle
  :: forall t m r.
     ( MonadHold t m, MonadFix m
     , MonadIO m, MonadIO (Performable m), PerformEvent t m)
  => MVar r -> Event t WSResponseMessage -> m (Event t [DataCircuit])
dataNetwork_effectEngine_handle _ wsResponseEvt = do
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

dataNetwork_effectEngine
  :: forall t m .
     (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Event t [DataCircuit]
  -> m (Event t [WSRequestMessage])
dataNetwork_effectEngine wsEvt = do
  divClass "ui segment basic" $
    divClass "ui grid" $ divClass "eight wide column" $ divClass "ui message" $ do
      elClass "h2" "ui header" $ do
        text "实效引擎"
      elClass "ul" "list" $ do
        el "li" $ elClass "h4" "ui header" $ text "包含常用函数组件库"
        el "li" $ elClass "h4" "ui header" $ text "运行逻辑碎片"
        el "li" $ elClass "h4" "ui header" $ text "可定制实现新实效引擎"        
        el "li" $ elClass "h4" "ui header" $ text ">_ Conduit"
        el "li" $ elClass "h4" "ui header" $ text ">_ PostgresSQL"
        el "li" $ elClass "h4" "ui header" $ text ">_ SparkSQL"        
        el "li" $ elClass "h4" "ui header" $ text ">_ KafkaSQL"
        el "li" $ elClass "h4" "ui header" $ text ">_ FlinkSQL"
        el "li" $ elClass "h4" "ui header" $ text ">_ Java"
        el "li" $ elClass "h4" "ui header" $ text ">_ R"
        el "li" $ elClass "h4" "ui header" $ text ">_ C"
        el "li" $ elClass "h4" "ui header" $ text ">_ 规则引擎"
        el "li" $ elClass "h4" "ui header" $ text ">_ 机器学习"

  divClass "ui segment basic" $ do
    elClass "table" "ui table collapsing" $ do
      theadUI
      tbodyUI
      tfootUI
  
  return never
