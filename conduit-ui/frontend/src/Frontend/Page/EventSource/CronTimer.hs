{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Frontend.Page.EventSource.CronTimer (eventSource_cronTimer) where

import Common.WebSocketMessage
import Prelude

import Reflex.Dom.Core
import Control.Monad (forM_, void)

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.String.Conversions (cs)
import Control.Monad.Fix (MonadFix)

eventSource_cronTimer
  :: forall t m .
     (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Dynamic t (Maybe WSResponseMessage)
  -> Event t WSResponseMessage
  -> m (Event t WSRequestMessage)
eventSource_cronTimer wsInit wsEvt = do
  let wsDyn = ffor wsInit $ \case
                Nothing -> []
                Just (WSResponseInit (AppST cronTimerSTs _)) -> cronTimerSTs
  elClass "table" "ui collapsing table" $ do
    el "thead" $ el "tr" $ do
      el "th" $ checkbox False def
      el "th" $ text "Cron表达式"
      el "th" $ text "名称"
    el "tbody" $ do
      elClass "tr" "warning" $ do
        el "td" $ elClass "i" "notched circle loading icon" blank
        newCronExpr <- el "td" $ inputElement def
        newCronName <- el "td" $ inputElement def
        return ()
      simpleList wsDyn $ \cronDyn -> do
        pb <- getPostBuild
        let cronEvt = updated cronDyn
        let cronBehavior = current cronDyn
        el "tr" $ do
          el "td" $ checkbox False def
          el "td" $ inputElement $ def 
            & inputElementConfig_setValue .~ leftmost
                [ fmap ce_expr cronEvt
                , tag (fmap ce_expr cronBehavior) pb]
          el "td" $ inputElement $ def
            & inputElementConfig_setValue .~ leftmost
                [ fmap ce_name cronEvt
                , tag (fmap ce_name cronBehavior)  pb]
        notReadyUntil pb
                
    el "tfoot" $ el "tr" $ do
      el "th" blank
      elAttr "th" ("colspan" =: "2") $ 
        elClass' "button" "ui small button teal" (text "删除")
  return never
