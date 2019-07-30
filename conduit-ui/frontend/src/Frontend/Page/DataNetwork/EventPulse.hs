{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}


module Frontend.Page.DataNetwork.EventPulse
  (dataNetwork_eventPulse_handle, dataNetwork_eventPulse) where

import Common.WebSocketMessage
import Common.Types
import Common.ExampleData
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

dataNetwork_eventPulse_handle
  :: forall t m r.
     ( MonadHold t m, MonadFix m
     , MonadIO m, MonadIO (Performable m), PerformEvent t m)
  => MVar r -> Event t WSResponseMessage
  -> m (Event t WSResponseMessage, Dynamic t [EventPulse])
dataNetwork_eventPulse_handle _ wsResponseEvt = do
  return (wsResponseEvt, constDyn exampleEventPulses)

theadUI
  :: forall t m .
     (DomBuilder t m, PostBuild t m)
  => m ()
theadUI = do
  el "thead" $ el "tr" $ do
    el "th"  $ divClass "ui fitted checkbox fields" $ checkbox False def >> el "label" blank
    el "th"  $ text "启用"
    el "th"  $ text "名称"
    el "th"  $ text "描述"
    el "th"  $ text "数据电路"

tbodyUI
  :: forall t m .
     (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
   => Dynamic t [EventPulse] -> m ()
tbodyUI wsDyn = do
  el "tbody" $ do
    elClass "tr" "" $ do
      el "td" $ elClass' "button" "ui circular icon button teal"  $ elClass "i" "plus icon" blank
      -- name
      el "td" $ divClass "ui fitted toggle checkbox" $ do
        checkbox True (def & checkboxConfig_setValue .~ (False <$ never))
        el "label" blank
      el "td" $ divClass "ui input" $ inputElement def
      -- description
      el "td" $ divClass "ui input" $ inputElement def
      -- stateContainer
      el "td" $ divClass "ui input" $ inputElement def
    simpleList wsDyn $ \pulseDyn -> do
      pb <- getPostBuild
      elDynAttr "tr" (constDyn M.empty) $ do
        deleteSelect <- el "td" $ divClass "ui fitted checkbox fields" $ do
          checkbox False (def & checkboxConfig_setValue .~ (False <$ never))
          el "label" $ blank
        el "th"  $ divClass "ui fitted toggle checkbox" $ do
          checkbox False (def & checkboxConfig_setValue .~ (tagPromptlyDyn (epEnable <$> pulseDyn) pb))
          el "label" blank
        elDynAttr "td" (constDyn M.empty) $ divClass "ui input" $
          inputElement $ def & inputElementConfig_setValue .~ leftmost
            [ updated pulseDyn <&> epName
            , tag (current pulseDyn <&> epName) pb ]
        elDynAttr "td" (constDyn M.empty) $ divClass "ui input" $ do
          inputElement $ def & inputElementConfig_setValue .~ leftmost
            [ updated pulseDyn <&> epDesc
            , tag (current pulseDyn <&> epDesc) pb ]
        elDynAttr "td" (constDyn M.empty) $ divClass "ui input" $ do
          inputElement $ def & inputElementConfig_setValue .~ leftmost
            [ updated pulseDyn <&> (cs . show . epDataConduitValues)
            , tag (current pulseDyn <&> (cs . show . epDataConduitValues)) pb ]
      
  return ()
      -- dataService

{--      
   el "td" $ divClass "ui input" $ inputElement def
    simpleList wsDyn $ \conduitDyn -> do
      pb <- getPostBuild
      elDynAttr "tr" (constDyn M.empty) $ do
        deleteSelect <- el "td" $ divClass "ui fitted checkbox fields" $ do
          checkbox False (def & checkboxConfig_setValue .~ (False <$ never))
          el "label" $ blank
        elDynAttr "td" (constDyn M.empty) $ divClass "ui input" $
          inputElement $ def & inputElementConfig_setValue .~ leftmost
            [ updated conduitDyn <&> dataCircuit_name
            , tag (current conduitDyn <&> dataCircuit_name) pb ]
--}

theadSecondUI
  :: forall t m .
     (DomBuilder t m, PostBuild t m)
  => m ()
theadSecondUI = do
  el "thead" $ el "tr" $ do
    el "th"  $ divClass "ui fitted checkbox fields" $ checkbox False def >> el "label" blank
    el "th"  $ text "启用"
    el "th"  $ text "名称"
    el "th"  $ text "描述"
    el "th"  $ text "数据电路"    
    el "th"  $ text "状态容器"
    el "th"  $ text "数据源"
    el "th"  $ text "数据服务"


tbodySecondUI
  :: forall t m .
     (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
   => Dynamic t [DataCircuitValue] -> m ()
tbodySecondUI wsDyn = do
  el "tbody" $ do
    elClass "tr" "" $ do
      el "td" $ elClass' "button" "ui circular icon button teal"  $ elClass "i" "plus icon" blank
      el "td" $ divClass "ui fitted toggle checkbox" $ do
        checkbox True (def & checkboxConfig_setValue .~ (False <$ never))
        el "label" blank
      el "td" $ divClass "ui input" $ inputElement def
      el "td" $ divClass "ui input" $ inputElement def
      el "td" $ divClass "ui input" $ inputElement def
      el "td" $ divClass "ui input" $ inputElement def
      el "td" $ divClass "ui input" $ inputElement def
      el "td" $ divClass "ui input" $ inputElement def      

  el "tbody" $ do
    simpleList wsDyn $ \valueDyn -> do
      pb <- getPostBuild
      elDynAttr "tr" (constDyn M.empty) $ do
        deleteSelect <- el "td" $ divClass "ui fitted checkbox fields" $ do
          checkbox False (def & checkboxConfig_setValue .~ (False <$ never))
          el "label" $ blank
        el "th"  $ divClass "ui fitted toggle checkbox" $ do
          checkbox False (def & checkboxConfig_setValue .~ (tagPromptlyDyn (dcivEnable <$> valueDyn) pb))
          el "label" blank
        elDynAttr "td" (constDyn M.empty) $ divClass "ui input" $
          inputElement $ def & inputElementConfig_setValue .~ leftmost
            [ updated valueDyn <&> dcivName
            , tag (current valueDyn <&> dcivName) pb ]
        elDynAttr "td" (constDyn M.empty) $ divClass "ui input" $
          inputElement $ def & inputElementConfig_setValue .~ leftmost
            [ updated valueDyn <&> dcivDesc
            , tag (current valueDyn <&> dcivDesc) pb ]
        elDynAttr "td" (constDyn M.empty) $ divClass "ui input" $
          inputElement $ def & inputElementConfig_setValue .~ leftmost
            [ updated valueDyn <&> snd . dcivLinkedDataCircuit
            , tag (current valueDyn <&> snd . dcivLinkedDataCircuit) pb ]
            
        elDynAttr "td" (constDyn M.empty) $ divClass "ui input" $ inputElement def
        elDynAttr "td" (constDyn M.empty) $ divClass "ui input" $ inputElement def
        elDynAttr "td" (constDyn M.empty) $ divClass "ui input" $ inputElement def        
          
  return ()        

dataNetwork_eventPulse
  :: forall t m .
     (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => (Event t WSResponseMessage, Dynamic t [EventPulse])
  -> m (Event t [WSRequestMessage])
dataNetwork_eventPulse (wsEvt, wsDyn) = do
  divClass "ui segment basic" $
    divClass "ui grid" $ divClass "eight wide column" $ divClass "ui message" $ do
      elClass "h2" "ui header" $ text "事件脉冲"
      elClass "ul" "list" $ do
        el "li" $ elClass "h4" "ui header" $ text "事件源触发事件脉冲"
        el "li" $ elClass "h4" "ui header" $ text "事件脉冲激活数据电路"

  divClass "ui segment basic" $ do
    elClass "table" "ui blue selectable table" $ theadUI >> tbodyUI wsDyn
  divClass "ui hidden divider" blank
  divClass "ui segment basic" $ do
    elClass "table" "ui blue selectable table" $ theadSecondUI >> tbodySecondUI (constDyn exampleDataConduitValues)
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
  return never



{--
  divClass "ui segment basic" $
    divClass "ui form" $ do
      myInput <- divClass "ui field" $ do
        textAreaElement $ def & initialAttributes .~ ("rows" =: "20")
                              & textAreaElementConfig_initialValue .~ (cs exampleCode)

      runEvt :: (Event t ()) <- divClass "ui field" $ do
        domEvent Click . fst <$> elClass' "button" "ui button teal" (text "RUN")

      divClass "ui field" $
        textAreaElement $ def & initialAttributes .~ ("rows" =: "10")
                              & textAreaElementConfig_initialValue .~ ""
                              & textAreaElementConfig_setValue .~ wsEvt
      return $ fmap (:[]) $ tagPromptlyDyn (fmap HaskellCodeRunRequest . value $ myInput) runEvt
--}
