{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend.Page.DataNetwork.LogicFragement
  (dataNetwork_logicFragement_handle, dataNetwork_logicFragement) where

import Common.WebSocketMessage
import Prelude

import Control.Monad (forM)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Reflex.Dom.Core

import Control.Concurrent (MVar)
import Data.String.Conversions (cs)

dataNetwork_logicFragement_handle
    :: forall t m r.
     ( MonadHold t m, MonadFix m
     , MonadIO m, MonadIO (Performable m), PerformEvent t m)
  => MVar r -> Event t WSResponseMessage
  -> m (Event t WSResponseMessage)
dataNetwork_logicFragement_handle wsST wsResponseEvt = do
  let wsEvt = ffilter (const True)
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
    elClass "th" "" $ text "调用接口"    
    elClass "th" "" $ text "ETL引擎"


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
        dropdown "PostgreSQL"
          (constDyn (  "SerDe" =: "SerDe"
                    <> "UDF" =: "UDF"
                    <> "UDAF" =: "UDAF"
                    <> "UDTF" =: "UDTF" ))
          def
      -- name
      el "td" $ divClass "ui mini input" $ inputElement def
      -- description
      el "td" $ divClass "ui mini input" $ inputElement def
      -- callInterface
      el "td" $ divClass "ui mini input" $
        inputElement (def & inputElementConfig_initialValue .~ "(#aaa, #bbb, #ccc) => #x  "
                          & initialAttributes .~ ("readonly" =: "")
                                              <> ("style" =: "background-color:pink;"))
      -- etlEngine
      el "td" $ divClass "ui mini input" $
        inputElement (def & inputElementConfig_initialValue .~ "Conduit; SQL"
                          & initialAttributes .~ ("readonly" =: "")
                                              <> ("style" =: "background-color:pink;"))

  return ()

tfootUI
  :: forall t m .
     (DomBuilder t m, PostBuild t m)
   => m ()
tfootUI = do
  el "tfoot" $ do
    blank

dataNetwork_logicFragement
  :: forall t m.
     ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => (Event t WSResponseMessage)
  -> m (Event t [WSRequestMessage])
dataNetwork_logicFragement wsEvt = do
  divClass "ui segment basic" $
    divClass "ui grid" $ divClass "eight wide column" $ divClass "ui message" $ do
      elClass "h2" "ui header" $ do
        text "逻辑碎片"
        
      elClass "ul" "list" $ do
        el "li" $ elClass "h4" "ui header" $ text "可复用的代码逻辑"
        el "li" $ elClass "h4" "ui header" $ text "可包含同类型子逻辑碎片"
        el "li" $ elClass "h4" "ui header" $ text "运行在实效引擎上"
        el "li" $ elClass "h4" "ui header" $ text "可使用实效引擎函数组件库"
  
  divClass "ui segment basic" $ do
    elClass "table" "ui table collapsing" $ do
      theadUI
      tbodyUI
      tfootUI

  divClass "ui segment basic" $ do
    divClass "ui grid"  $ divClass "twelve wide column" $ do
      elClass "h4" "ui horizontal divider header" $ do
        text "逻辑碎片详情"
    divClass "ui grid" $ divClass "eight wide column" $ do
        divClass "ui segment basic" $ do
          elClass "table" "ui table" $ do
            el "thead" $ el "tr" $ do
              elClass "th" "" $ checkbox False def
              elClass "th" "" $ text "名称"
              elClass "th" "" $ text "类型"
              elClass "th" "" $ text "描述"
            el "tbody" $  do
              el "tr" $ do
                el "td" $ checkbox False def
                el "td" $ text "groupByFields"
                el "td" $ text "text"
                el "td" $ text "NULL"
              el "tr" $ do
                el "td" $ checkbox False def
                el "td" $ text "bbb"
                el "td" $ text "text"
                el "td" $ text "NULL"
              el "tr" $ do
                el "td" $ checkbox False def
                el "td" $ text "ccc"
                el "td" $ text "text"
                el "td" $ text "NULL"
    divClass "ui segment basic" $ do
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
