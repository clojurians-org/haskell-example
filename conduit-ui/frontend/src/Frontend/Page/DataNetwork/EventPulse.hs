{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}


module Frontend.Page.DataNetwork.EventPulse
  (dataNetwork_eventPulse_handle, dataNetwork_eventPulse) where

import Common.WebSocketMessage
import Common.Types.DataNetwork
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


exampleEventPulses :: [EventPulse]
exampleEventPulses = do
  [ def { eventPulse_name = "EP_DWJobNotify"
        , eventPulse_desc = "数仓作业通知"
        , eventPulse_dataConduitValues = [] }
    ]
exampleDataConduitValues :: [DataCircuitValue]
exampleDataConduitValues = do
  [ def { dataCircuitValue_name = "DCV_HRFilePush"
        , dataCircuitValue_desc = "华瑞银行数据下传平台"
        , dataCircuitValue_linkedDataCircuit = (7, "文件下传平台")}
    ]
    
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
          checkbox False (def & checkboxConfig_setValue .~ (tagPromptlyDyn (eventPulse_enable <$> pulseDyn) pb))
          el "label" blank
        elDynAttr "td" (constDyn M.empty) $ divClass "ui input" $
          inputElement $ def & inputElementConfig_setValue .~ leftmost
            [ updated pulseDyn <&> eventPulse_name
            , tag (current pulseDyn <&> eventPulse_name) pb ]
        elDynAttr "td" (constDyn M.empty) $ divClass "ui input" $ do
          inputElement $ def & inputElementConfig_setValue .~ leftmost
            [ updated pulseDyn <&> eventPulse_desc
            , tag (current pulseDyn <&> eventPulse_desc) pb ]
        elDynAttr "td" (constDyn M.empty) $ divClass "ui input" $ do
          inputElement $ def & inputElementConfig_setValue .~ leftmost
            [ updated pulseDyn <&> (cs . show . eventPulse_dataConduitValues)
            , tag (current pulseDyn <&> (cs . show . eventPulse_dataConduitValues)) pb ]
      
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
          checkbox False (def & checkboxConfig_setValue .~ (tagPromptlyDyn (dataCircuitValue_enable <$> valueDyn) pb))
          el "label" blank
        elDynAttr "td" (constDyn M.empty) $ divClass "ui input" $
          inputElement $ def & inputElementConfig_setValue .~ leftmost
            [ updated valueDyn <&> dataCircuitValue_name
            , tag (current valueDyn <&> dataCircuitValue_name) pb ]
        elDynAttr "td" (constDyn M.empty) $ divClass "ui input" $
          inputElement $ def & inputElementConfig_setValue .~ leftmost
            [ updated valueDyn <&> dataCircuitValue_desc
            , tag (current valueDyn <&> dataCircuitValue_desc) pb ]
        elDynAttr "td" (constDyn M.empty) $ divClass "ui input" $
          inputElement $ def & inputElementConfig_setValue .~ leftmost
            [ updated valueDyn <&> snd . dataCircuitValue_linkedDataCircuit
            , tag (current valueDyn <&> snd . dataCircuitValue_linkedDataCircuit) pb ]
            
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
