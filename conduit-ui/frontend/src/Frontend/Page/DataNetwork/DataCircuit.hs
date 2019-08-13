{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Frontend.Page.DataNetwork.DataCircuit
  (dataNetwork_dataCircuit) where

import Common.Types.DataNetwork
import Common.Types.DataSandbox
import Common.ExampleData
import Common.WebSocketMessage
import Prelude

import Reflex.Dom.Core
import Frontend.Class (ToUI(toLabel, toIcon, toDOM))

import Text.Heredoc (str)
import GHC.Generics (Generic)
import Control.Monad (forM, forM_, void)
import Control.Monad.Fix (MonadFix)

import qualified Data.Text as T
import qualified Data.Map as M
import Data.String.Conversions (cs)

import Data.Functor ((<&>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent (MVar, newMVar, putMVar, modifyMVar, modifyMVar_, readMVar, threadDelay)
import qualified Data.Tree as TR

dataCircuitDOM :: DomBuilder t m
  => TR.Tree DataCircuitPart -> m ()
dataCircuitDOM rootNode =
  divClass "ui item" $ do
    elClass "i" "random icon" blank
    divClass "content" $ do
      divClass "header" $ text "DataCircuit-数据电路"
    toDOM undefined rootNode
  
dataNetwork_dataCircuit_handle
  :: forall t m r.
     ( MonadHold t m, MonadFix m
     , MonadIO m, MonadIO (Performable m), PerformEvent t m)
  => MVar r -> Event t WSResponseMessage -> m (Event t WSResponseMessage, Dynamic t [DataCircuit])
dataNetwork_dataCircuit_handle _ wsResponseEvt = do
  return (wsResponseEvt, constDyn exampleDataCircuits)

theadUI
  :: forall t m .
     (DomBuilder t m, PostBuild t m)
  => m ()
theadUI = do
  el "thead" $ el "tr" $ do
    el "th"  $ divClass "ui fitted checkbox fields" $ checkbox False def >> el "label" blank
    el "th"  $ text "名称"
    el "th"  $ text "描述"
    el "th"  $ text "状态容器"
    el "th"  $ text "数据源"
    el "th"  $ text "数据服务"       
--    elClass "th" "" $ text "子数据电路"
--    elClass "th" "" $ text "数据导管"
--    elClass "th" "" $ text "部件组合器"

    {--
    elClass "th" "" $ text "配置数据模式"    
    elClass "th" "" $ text "请求数据模式"
    elClass "th" "" $ text "响应数据模式"

    --}

tbodyUI
  :: forall t m .
     (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
   => Dynamic t [DataCircuit] -> m ()
tbodyUI wsDyn = do
  el "tbody" $ do
    elClass "tr" "" $ do
      el "td" $ elClass' "button" "ui circular icon button teal"  $ elClass "i" "plus icon" blank
      -- name
      el "td" $ divClass "ui input" $ inputElement def
      -- description
      el "td" $ divClass "ui input" $ inputElement def
      -- stateContainer
      el "td" $ divClass "ui input" $ inputElement def
      -- dataSource
      el "td" $ divClass "ui input" $ inputElement def
      -- dataService
      el "td" $ divClass "ui input" $ inputElement def
    simpleList wsDyn $ \conduitDyn -> do
      pb <- getPostBuild
      elDynAttr "tr" (constDyn M.empty) $ do
        deleteSelect <- el "td" $ divClass "ui fitted checkbox fields" $ do
          checkbox False (def & checkboxConfig_setValue .~ (False <$ never))
          el "label" $ blank
        elDynAttr "td" (constDyn M.empty) $ divClass "ui input" $
          inputElement $ def & inputElementConfig_setValue .~ leftmost
            [ updated conduitDyn <&> dciName
            , tag (current conduitDyn <&> dciName) pb ]
        elDynAttr "td" (constDyn M.empty) $ divClass "ui input" $ do
          inputElement $ def & inputElementConfig_setValue .~ leftmost
            [ updated conduitDyn <&> dciDesc
            , tag (current conduitDyn <&> dciDesc) pb ]
        elDynAttr "td" (constDyn M.empty) $ divClass "ui input" $ do
          inputElement $ def & inputElementConfig_setValue .~ leftmost
            [ updated conduitDyn <&> cs . show . dsahStateContainers . dciDataSandboxHolder
            , tag (current conduitDyn <&> cs . show . dsahStateContainers . dciDataSandboxHolder) pb ]
        elDynAttr "td" (constDyn M.empty) $ divClass "ui input" $ do
          inputElement $ def & inputElementConfig_setValue .~ leftmost
            [ updated conduitDyn <&> cs . show . dsahDataSources . dciDataSandboxHolder
            , tag (current conduitDyn <&> cs . show . dsahDataSources . dciDataSandboxHolder) pb ]
        elDynAttr "td" (constDyn M.empty) $ divClass "ui input" $ do
          inputElement $ def & inputElementConfig_setValue .~ leftmost
            [ updated conduitDyn <&> cs . show . dsahDataServices . dciDataSandboxHolder
            , tag (current conduitDyn <&> cs . show . dsahDataServices . dciDataSandboxHolder) pb ]            
        blank
  return ()

dataNetwork_dataCircuit
  :: forall t m .
     (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => (Event t WSResponseMessage, Dynamic t [DataCircuit])
  -> m (Event t [WSRequestMessage])
dataNetwork_dataCircuit (wsEvt, wsDyn)  = do
  divClass "ui segment basic" $
    divClass "ui grid" $ divClass "eight wide column" $ divClass "ui message" $ do
      elClass "h2" "ui header" $ do
        text "数据电路"
      elClass "ul" "list" $ do
        el "li" $ elClass "h4" "ui header" $ text "由数据导管连接而成"
        el "li" $ elClass "h4" "ui header" $ text "可包含子数据电路"
        el "li" $ elClass "h4" "ui header" $ text "按需构建业务系统"
        el "li" $ elClass "h4" "ui header" $ text "分布式调度"

  divClass "ui segment basic" $ do
    elClass "table" "ui blue selectable table" $ theadUI >> tbodyUI wsDyn
    divClass "" $ do
      divClass "ui top attached warning segment" $ do
        divClass "ui horizontal divided list" $ do
          divClass "item" $ do
            elClass "i" "angle double down icon" blank
            divClass "content" $ divClass "header" $ text "成功继续下一步"
          divClass "item" $ do
            elClass "i" "arrows alternate vertical icon" blank
            divClass "content" $ divClass "header" $ text "失败尝试下一步"       
          divClass "item" $ do
            elClass "i" "code icon" blank
            divClass "content" $ divClass "header" $ text "独立并行逻辑"
      divClass "ui attached segment" $ divClass "ui grid" $ divClass "eight wide column" $ do
        divClass "ui list" $ do
          divClass "ui item" $ do
            elClass "i" "tags icon" blank
            divClass "content" $ do
              divClass "header" $ text "MonadState-状态容器"
              divClass "list" $ do
                divClass "item" $ do
                  elClass "i" "code icon" blank
                  divClass "content" $ divClass "header" $ text "SQLCursor"
                
          divClass "ui item" $ do
            elClass "i" "database icon" blank
            divClass "content" $ do
              divClass "header" $ text "MonadReader-数据源"
              divClass "list" $ do
                divClass "item" $ do
                  elClass "i" "code icon" blank
                  divClass "content" $ divClass "header" $ text "RestAPI"
      
          divClass "ui item" $ do
            elClass "i" "wifi icon" blank
            divClass "content" $ do
              divClass "header" $ text "MonadWriter-数据服务"
              divClass "list" $ do
                divClass "item" $ do
                  elClass "i" "code icon" blank
                  divClass "content" $ divClass "header" $ text "NotifyService_WebHook"
          divClass "ui item" $ do
            elClass "i" "paperclip icon" blank
            divClass "content" $ do
              divClass "header" $ text "MonadFix-组件引用"
              divClass "list" $ do
                divClass "item" $ do
                  elClass "i" "folder icon" blank
                  divClass "content" $ divClass "header" $ text "数据导管"
                divClass "item" $ do
                  elClass "i" "folder icon" blank
                  divClass "content" $ divClass "header" $ text "逻辑碎片"
          dataCircuitDOM examplePartCombinator
                      
    divClass "ui hidden divider" blank
    divClass "" $ do
      divClass "ui top attached segment" $ do
        elClass "h4" "ui header" $ text "代码浏览器"
      divClass "ui attached segment" $ divClass "ui form field" $ do
        textAreaElement $ def
          & initialAttributes .~ ("rows" =: "20")
          & textAreaElementConfig_initialValue .~ exampleConduitCode
       

  return never
