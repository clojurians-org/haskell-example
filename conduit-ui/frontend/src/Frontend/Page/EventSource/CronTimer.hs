{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

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
  => Event t WSResponseMessage
  -> m (Event t WSRequestMessage)
eventSource_cronTimer wsResponseEvt = do
  wsDyn <- foldDyn (\wsMsg xs -> case wsMsg of
                       WSInitResponse (AppST cronTimers)  -> cronTimers ++ xs
                       CronTimerCreateResponse (Right cronTimer) -> cronTimer : xs
                       CronTimerUpdateResponse (Right cronTimer) -> cronTimer : xs
                       _ -> xs)
             [] wsResponseEvt
  elClass "table" "ui collapsing table" $ do
    el "thead" $ el "tr" $ do
      el "th" $ checkbox False def
      el "th" $ text "Cron表达式"
      el "th" $ text "名称"
    (createEvt, updateEvt) <- 
      el "tbody" $ do
        createEvt' <- 
          elClass "tr" "warning" $ do
            el "td" $ elClass "i" "notched circle loading icon" blank
            rec
              let createEvt = leftmost $ [ keypress Enter newCronExpr
                                         , keypress Enter newCronName]
              newCronExpr <- el "td" $ inputElement def
              newCronName <- el "td" $ inputElement def
            return $ flip tag createEvt $ fmap CronTimerCreateRequest $
                  CronTimer
              <$> (current . value) newCronName
              <*> (current . value) newCronExpr
              <*> return Nothing
        updateEvt' <- 
          fmap (switchDyn . fmap leftmost) $ simpleList wsDyn $ \cronDyn -> do
            pb <- getPostBuild
            let cronEvt = updated cronDyn
            let cronBehavior = current cronDyn
            el "tr" $ do
              rec
                let updateCronTimerEvt =
                      leftmost [ keypress Enter updateCronExpr
                               , keypress Enter updateCronExpr]
                el "td" $ checkbox False def
                updateCronExpr <- el "td" $ inputElement $ def 
                  & inputElementConfig_setValue .~ leftmost
                      [ fmap ce_expr cronEvt
                      , tag (fmap ce_expr cronBehavior) pb]
                updateCronName <- el "td" $ inputElement $ def
                  & inputElementConfig_setValue .~ leftmost
                      [ fmap ce_name cronEvt
                      , tag (fmap ce_name cronBehavior)  pb]
              return $ flip tag updateCronTimerEvt $ fmap CronTimerUpdateRequest $
                    CronTimer
                <$> (current . value) updateCronName
                <*> (current . value) updateCronExpr
                <*> fmap ce_xid cronBehavior
        return (createEvt', updateEvt')
    deleteEvt <- el "tfoot" $ el "tr" $ do
      el "th" blank
      elAttr "th" ("colspan" =: "2") $ 
        elClass' "button" "ui small button teal" (text "删除")
      return never
    return $ leftmost [createEvt, updateEvt, deleteEvt]
