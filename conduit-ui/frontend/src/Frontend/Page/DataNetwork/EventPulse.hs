{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}


module Frontend.Page.DataNetwork.EventPulse
  (dataNetwork_eventPulse) where

import Common.WebSocketMessage
import Common.Types
import Common.ExampleData
import Frontend.FrontendStateT
import Frontend.Widget
import Frontend.Class
import Prelude

import Reflex.Dom.Core
import Control.Monad (forM_, void)
import Control.Monad.Fix (MonadFix)

import Data.Functor ((<&>))
import qualified Data.Text as T
import qualified Data.Map as M
import Data.String.Conversions (cs)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent (MVar, newMVar, putMVar, modifyMVar, modifyMVar_, readMVar, threadDelay)
import Control.Lens hiding (lens)
import Labels

trEventPulse :: forall t m. (DomBuilder t m, PostBuild t m)
  => Dynamic t EventPulse -> m ()
trEventPulse eventPulseD = do
  tdDyn (eventPulseD <&> (cs . show . epEnable))
  tdDyn (eventPulseD <&> epName)
  tdDyn (eventPulseD <&> epDesc)
  return ()

trDataCircuitValue :: forall t m. (DomBuilder t m, PostBuild t m)
  => Dynamic t DataCircuitValue -> m ()
trDataCircuitValue dcivD = do
  tdDyn (dcivD <&> (cs . show . dcivEnable))
  tdDyn (dcivD <&> dcivName)
  tdDyn (dcivD <&> dcivDesc)
  tdDyn (dcivD <&> snd . dcivLinkedDataCircuit)  
  return ()


dataNetwork_eventPulse
  :: forall t m .
     ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m
     , HasFrontendState t (FaaSCenter, WSResponseMessage) m)
  =>  m ()
dataNetwork_eventPulse = do
  (stD, msgD) :: (Dynamic t FaaSCenter, Dynamic t WSResponseMessage) <- splitDynPure <$> askFrontendState
  let epsD = stD <&> (^.. lens #dataNetwork . lens #eventPulses . each)

  divClass "ui segment basic" $ do
    pageHeader "事件脉冲" ["事件源触发事件脉冲", "事件脉冲激活数据电路"]

  epE <- divClass "ui segment basic" $ do
    elClass "table" "ui selectable table" $ do
      theadList ["启用", "名称", "描述"]
      e0 <- (trE $ createIcon >> trEventPulse (constDyn (def :: EventPulse))) <&> tagPromptlyDyn (return def)
      e1 <- switchDyn . fmap leftmost <$> simpleList epsD
              (\x -> (trE $ selectE >> trEventPulse x) <&> tagPromptlyDyn x)      
      return $ leftmost [e0, e1]

  dcivsD <- holdDyn [] (epE <&> epDataCircuitValues)
  divClass "ui hidden divider" blank

  divClass "ui segment basic" $ do
    elClass "table" "ui selectable table" $ do
      theadList ["启用", "名称", "描述", "数据电路"]
      e0 <- (trE $ createIcon >> trDataCircuitValue (constDyn (def :: DataCircuitValue))) <&> tagPromptlyDyn (return (def::DataCircuitValue))
      e1 <- switchDyn . fmap leftmost <$> simpleList dcivsD
              (\x -> (trE $ selectE >> trDataCircuitValue x) <&> tagPromptlyDyn x)
      return $ leftmost [e0, e1]
      
  divClass "ui hidden divider" blank
  divClass "ui segment basic" $ do
      divClass "ui top attached segment" $ do
        elClass "h4" "ui header" $ text "代码浏览器"
      divClass "ui attached segment" $ divClass "ui form" $ do
        divClass "three fields" $ do
          divClass "field" $ do
            el "label" $ text "状态容器"
            inputElement $ def
          divClass "field" $ do
            el "label" $ text "数据源"
            inputElement $ def
          divClass "field" $ do
            el "label" $ text "数据服务"
            inputElement $ def
        divClass "field" $ do
          el "label" $ text "函数服务"
          textAreaElement $ def
            & initialAttributes .~ ("rows" =: "20")
            & textAreaElementConfig_initialValue .~ ""
        divClass "field" $ do
          elClass' "button" "ui button blue" (text "实时激活")
        divClass "ui message" $ do
--          divClass "header" $ text "运行结果"
          text ""
  return ()
