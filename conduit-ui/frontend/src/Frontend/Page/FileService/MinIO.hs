{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend.Page.DataSource.SQLCursor
  (dataSource_sqlCursor_handle, dataSource_sqlCursor) where

import Common.WebSocketMessage
import Prelude

import Control.Monad (forM)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Reflex.Dom.Core

import Control.Concurrent (MVar)
import Data.String.Conversions (cs)

dataSource_sqlCursor_handle
    :: forall t m r.
     ( MonadHold t m, MonadFix m
     , MonadIO m, MonadIO (Performable m), PerformEvent t m)
  => MVar r -> Event t WSResponseMessage
  -> m (Event t WSResponseMessage)
dataSource_sqlCursor_handle wsST wsResponseEvt = do
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
    elClass "th" "" $ text "数据源名称"
    elClass "th" "" $ text "数据源类型"
    elClass "th" "" $ text "主机"
    elClass "th" "" $ text "数据库"
    elClass "th" "" $ text "数据表"

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
      -- type
      el "td" $ divClass "ui mini input" $
        dropdown "PostgreSQL"
          (constDyn (  "PostgreSQL" =: "PostgreSQL"
                  <> "Oracle" =: "Oracle"
                  <> "MySQL" =: "MySQL" ))
          def
      -- host
      el "td" $ divClass "ui mini input" $ inputElement def
      -- database
      el "td" $ divClass "ui mini input" $ inputElement def
      -- table
      el "td" $ divClass "ui mini input" $
        inputElement (def & inputElementConfig_initialValue .~ "aaa"
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

dataSource_sqlCursor
  :: forall t m.
     ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => (Event t WSResponseMessage)
  -> m (Event t [WSRequestMessage])
dataSource_sqlCursor wsEvt = do
  divClass "ui segment basic" $ do
    elClass "table" "ui table collapsing" $ do
      theadUI
      tbodyUI
      tfootUI

  divClass "ui segment basic" $ do
    divClass "ui grid"  $ divClass "twelve wide column" $ do
      elClass "h4" "ui horizontal divider header" $ do
        text "数据源详情"
      divClass "ui form compact" $ do
        divClass "fields" $ do
          divClass "field" $ do
            el "label" $ text "用户名"
            inputElement def
          divClass "field" $ do
            el "label" $ text "密码"
            inputElement def
          divClass "field" $ do
            el "label" $ elClass "i" "angle double down icon" blank
            elClass' "button" "ui button teal" (text "连接")
  divClass "ui segment basic" $ do
    divClass "ui grid" $ do
      divClass "four wide column" $ do
        divClass "ui segment" $ do
          divClass "ui mini icon input" $ do
            inputElement def
            elClass "i" "circular search link icon" blank
          divClass "ui list" $ do
            divClass "item" $ do
              elClass "i" "database icon" blank
              divClass "content" $ 
                elClass "h4" "ui header" $ text "information_schema"
              divClass "list" $ do
                divClass "item" $ do
                  elClass "i" "expand icon" blank
                  divClass "content" $ el "a" $ text "to_be_fill"
              elClass "i" "database icon" blank
              divClass "content" $
                elClass "h4" "ui header" $ text "public"
              divClass "list" $ do
                divClass "item" $ do
                  elClass "i" "expand icon" blank
                  divClass "content" $ el "a" $ text "hello"
                divClass "item" $ do
                  elClass "i" "expand icon" blank
                  divClass "content" $ el "a" $ text "world"
  
      divClass "eight wide column" $ do
          elClass "table" "ui table" $ do
            el "thead" $ el "tr" $ do
              elClass "th" "" $ checkbox False def
              elClass "th" "" $ text "名称"
              elClass "th" "" $ text "类型"
              elClass "th" "" $ text "描述"
            el "tbody" $  do
              el "tr" $ do
                el "td" $ checkbox False def
                el "td" $ text "name"
                el "td" $ text "text"
                el "td" $ text "NULL"
              el "tr" $ do
                el "td" $ checkbox False def
                el "td" $ text "expr"
                el "td" $ text "text"
                el "td" $ text "NULL"
              el "tr" $ do
                el "td" $ checkbox False def
                el "td" $ text "type"
                el "td" $ text "text"
                el "td" $ text "NULL"
                
  return never
