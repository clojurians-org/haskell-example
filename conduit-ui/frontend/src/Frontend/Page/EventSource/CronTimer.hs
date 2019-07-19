{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Page.EventSource.CronTimer (eventSource_cronTimer) where

import Prelude
import Reflex.Dom.Core

import Control.Monad (forM_)

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.String.Conversions (cs)

import Common.WebSocketMessage


exampleAppST :: AppST
exampleAppST = MkAppST
  [ CronEventDef "larluo1" "*/5 * * *" 1
  , CronEventDef "larluo2" "*/5 * * *" 2]
  []
  
eventSource_cronTimer
  :: forall t m .DomBuilder t m
  => Event t B.ByteString -> m (Event t [T.Text])
eventSource_cronTimer wsEvt = do
  MkAppST cronEventDefs _  <- exampleAppST <$ button "test"
  elClass "table" "ui collapsing table" $ do
    el "thead" $ el "tr" $
      forM_ ["Cron表达式", "事件名称"] (el "th" . text)
    el "tbody" $ 
      forM_ cronEventDefs $ \(CronEventDef name expr id) ->
        el "tr" $ do
          el "td" $ text expr
          el "td" $ text name
--          el "td" $ text (cs $ show id)

  return never
