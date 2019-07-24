{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend.Page.DataNetwork.DataConduit
  (dataNetwork_dataConduit_handle, dataNetwork_dataConduit) where

import Common.WebSocketMessage
import Prelude

import Control.Monad (forM)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Reflex.Dom.Core

import Control.Concurrent (MVar)
import Data.String.Conversions (cs)

dataNetwork_dataConduit_handle
    :: forall t m r.
     ( MonadHold t m, MonadFix m
     , MonadIO m, MonadIO (Performable m), PerformEvent t m)
  => MVar r -> Event t WSResponseMessage
  -> m (Event t WSResponseMessage)
dataNetwork_dataConduit_handle wsST wsResponseEvt = do
  let wsEvt = ffilter (isWSInitResponse
                   ||| isSQLCursorCreateResponse
                   ||| isSQLCursorUpdateResponse
                   ||| isSQLCursorDeleteResponse
                   ||| isSQLCursorDatabaseReadResponse
                   ||| isSQLCursorTableReadResponse)
                wsResponseEvt
  return wsEvt

theadUI
  :: forall t m .
     (DomBuilder t m, PostBuild t m)
  => m ()
theadUI = do
  el "thead" $ el "tr" $ do
    elClass "th" "" $ checkbox False def
    elClass "th" "" $ text "类型"
    elClass "th" "" $ text "名称"
    elClass "th" "" $ text "描述"
    elClass "th" "" $ text "数据源"
    elClass "th" "" $ text "状态容器"    
    elClass "th" "" $ text "数据服务"

tbodyUI
  :: forall t m .
     (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
   => m ()
tbodyUI = do
  el "tbody" $ do
    elClass "tr" "warning" $ do
      el "td" $ elClass "i" "notched circle loading icon" blank 
      -- type
      el "td" $ divClass "ui mini input" $
        dropdown "streaming"
          (constDyn (  "streaming" =: "实时"
                    <> "batch" =: "批量" ))
          def
      -- name
      el "td" $ divClass "ui mini input" $ inputElement def
      -- description
      el "td" $ divClass "ui mini input" $ inputElement def
      -- dataSource
      el "td" $ divClass "ui mini input" $ inputElement def
      -- dataService
      el "td" $ divClass "ui mini input" $
        inputElement def
      -- stateContainer
      el "td" $ divClass "ui mini input" $ do
        inputElement def

  return ()

tfootUI
  :: forall t m .
     (DomBuilder t m, PostBuild t m)
   => m ()
tfootUI = do
  el "tfoot" $ do
    blank

dataNetwork_dataConduit
  :: forall t m.
     ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => (Event t WSResponseMessage)
  -> m (Event t [WSRequestMessage])
dataNetwork_dataConduit wsEvt = do
  divClass "ui segment basic" $ do
    divClass "ui message compact" $ do
      elClass "h2" "ui header" $ do
        text "数据导管"
      elClass "ul" "list" $ do
        el "li" $ elClass "h4" "ui header" $ text "由逻辑碎片复合而成"
        el "li" $ elClass "h4" "ui header" $ text "可读取多个数据源"
        el "li" $ elClass "h4" "ui header" $ text "可推送多个数据服务"
  divClass "ui segment basic" $ do
    elClass "table" "ui table collapsing" $ do
      theadUI
      tbodyUI
      tfootUI

  divClass "ui segment basic" $ do
    divClass "ui grid"  $ divClass "twelve wide column" $ do
      elClass "h4" "ui horizontal divider header" $ do
        text "数据导管详情"
    divClass "ui grid" $ divClass "eight wide column" $ do
      divClass "ui list" $ do
        divClass "ui item" $ do
          elClass "i" "angle double down icon" blank
          divClass "content" $ do
            divClass "header" $ text "DataSource"
            divClass "description" $ text "数据源"
--            divClass "ui item" $ do
--              divClass "i
        divClass "ui item" $ do
          elClass "i" "angle double down icon" blank
          divClass "content" $ do
            divClass "header" $ text "DataPipe"
            divClass "description" $ text "数据管道"
        divClass "ui item" $ do
          elClass "i" "angle double down icon" blank
          divClass "content" $ do
            divClass "header" $ text "DataService"
            divClass "description" $ text "数据服务 "
        
    divClass "ui grid" $ divClass "ten wide column" $ do        
        divClass "ui top attached warning segment" $ do
          divClass "ui grid" $ do
            divClass "four wide column" $ do
              divClass "ui toggle checkbox" $ do
                checkbox True def
                el "label" $ text "Conduit"
            divClass "four wide column" $ do
              divClass "ui toggle checkbox" $ do
                checkbox False def
                el "label" $ text "SQL"
        divClass "ui attached segment" $ do
          divClass "ui segment basic" $ divClass "ui list" $ do
            divClass "item" $ do
              divClass "ui form" $ do
                divClass "field" $ do
                  el "label" $ text "Conduit表达式"
                  textAreaElement $ def & initialAttributes .~ ("rows" =: "5")
                                  & textAreaElementConfig_initialValue .~ "max <$> C.max #a <*> C.max #b <*> C.max #c"
            
            divClass "item" $ do
              divClass "ui form" $ do
                divClass "field" $ do
                  el "label" $ text "SQL表达式-3"
                  textAreaElement $ def & initialAttributes .~ ("rows" =: "5")
                                  & textAreaElementConfig_initialValue .~ (cs . unlines)
                                      [  "SELECT t1.* FROM ("
                                       , "  SELECT t0.*, {concate ',' #groupFields}, row_number() OVER PARTITION BY (#groupFields)"
                                       , "  FROM #inData t0"
                                       , ") t1 where rn = 1" ]
  return never
