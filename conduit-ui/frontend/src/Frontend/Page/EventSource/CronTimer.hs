{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend.Page.EventSource.CronTimer
  (eventSource_cronTimer_handle, eventSource_cronTimer) where

import Common.WebSocketMessage
import Prelude

import Reflex.Dom.Core
import Control.Monad (forM_, void, guard)

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Map as M
import Data.String.Conversions (cs)
import Control.Monad.Fix (MonadFix)

import Labels ((:=)(..), Has, get, set)
import Control.Concurrent (MVar, newMVar, putMVar, modifyMVar, modifyMVar_, readMVar, threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.List (sortOn)

eventSource_cronTimer_handle
  :: forall t m r.
     ( MonadHold t m, MonadFix m
     , MonadIO m, MonadIO (Performable m), PerformEvent t m
     , Has "eventSource_cronTimer" [CronTimer] r)
  => MVar r -> Event t WSResponseMessage -> m (Dynamic t [CronTimer])
eventSource_cronTimer_handle wsST wsResponseEvt = do
  myST <- liftIO $ readMVar wsST
  wsDyn <- foldDyn (\wsMsg xs -> case wsMsg of
                       WSInitResponse (AppST cronTimers)  -> cronTimers ++ xs
                       CronTimerCreateResponse (Right cronTimer) -> cronTimer : xs
                       CronTimerUpdateResponse (Right cronTimer) -> cronTimer : filter (not . isSameCronXID cronTimer) xs
                       _ -> xs)
             (get #eventSource_cronTimer myST) wsResponseEvt
  performEvent $ do
    ffor (updated wsDyn) $ \xs ->  do
      liftIO $ modifyMVar_ wsST $ return . set #eventSource_cronTimer xs
  return (fmap (sortOn ce_name) wsDyn)

keypressExclude :: (Reflex t, HasDomEvent t e 'KeypressTag, DomEventType e 'KeypressTag ~ Word) => Key -> e -> Event t ()
keypressExclude key = fmapMaybe (\n -> guard $ keyCodeLookup (fromIntegral n) /= key) . domEvent Keypress

eventSource_cronTimer
  :: forall t m.
     ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Dynamic t [CronTimer]
  -> m (Event t WSRequestMessage)
eventSource_cronTimer wsDyn = do
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
            elDynAttr "tr" (ffor cronDyn $ \(CronTimer _ _ xid) -> "id" =: (cs . show $ xid)) $ do
              rec
                let adjustCronExprNoEnterEvt = keypressExclude Enter updateCronExpr
                let adjustCronNameNoEnterEvt = keypressExclude Enter updateCronName
                let cronExprEnterEvt = keypress Enter updateCronExpr
                let cronNameEnterEvt = keypress Enter updateCronName
                let updateCronTimerEvt = leftmost [cronExprEnterEvt, cronNameEnterEvt]

                conrExprDyn <- holdDyn M.empty $ leftmost
                                 [ ("bgcolor" =: "yellow") <$ adjustCronExprNoEnterEvt
                                 , M.empty <$ cronExprEnterEvt]
                conrNameDyn <- holdDyn M.empty $ leftmost
                                 [ ("bgcolor" =: "yellow") <$ adjustCronNameNoEnterEvt
                                 , M.empty <$ cronNameEnterEvt]

                
                el "td" $ checkbox False def
                updateCronExpr <- elDynAttr "td" conrExprDyn $ inputElement $ def 
                  & inputElementConfig_setValue .~ leftmost
                      [ fmap ce_expr cronEvt
                      , tag (fmap ce_expr cronBehavior) pb]
                updateCronName <- elDynAttr "td" conrNameDyn $ inputElement $ def
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
