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
import Data.Maybe (fromJust)

import Labels ((:=)(..), Has, get, set)
import Control.Concurrent (MVar, newMVar, putMVar, modifyMVar, modifyMVar_, readMVar, threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty(..), toList)

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

keydownExclude :: (Reflex t, HasDomEvent t e 'KeydownTag, DomEventType e 'KeydownTag ~ Word) => Key -> e -> Event t ()
keydownExclude key = fmapMaybe (\n -> guard $ keyCodeLookup (fromIntegral n) /= key) . domEvent Keydown

eventSource_cronTimer
  :: forall t m.
     ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Dynamic t [CronTimer]
  -> m (Event t [WSRequestMessage])
eventSource_cronTimer wsDyn = do
  elClass "table" "ui collapsing table" $ do
    el "thead" $ el "tr" $ do
      el "th" $ checkbox False def
      el "th" $ text "Cron表达式"
      el "th" $ text "名称"
    cronTimerEvt <- 
      el "tbody" $ do
        createEvt <- 
          elClass "tr" "warning" $ do
            el "td" $ elClass "i" "notched circle loading icon" blank
            rec
              let cronExprEnterEvt = keypress Enter newCronExpr
              let cronNameEnterEvt = keypress Enter newCronName
              let mkCreateRequest =
                    fmap CronTimerCreateRequest $ CronTimer
                      <$> value newCronName
                      <*> value newCronExpr
                      <*> return Nothing

              newCronExpr <- el "td" $ inputElement def
              newCronName <- el "td" $ inputElement def
            return $ tagPromptlyDyn mkCreateRequest (leftmost [cronExprEnterEvt, cronNameEnterEvt])

        updateDeleteEvt  <- 
          fmap (switchDyn . fmap (mergeWith (<>))) $ simpleList wsDyn $ \cronDyn -> do          
            pb <- getPostBuild
            let cronEvt = updated cronDyn
            let cronBehavior = current cronDyn
            elDynAttr "tr" (ffor cronDyn $ \(CronTimer _ _ xid) -> "id" =: (cs . show $ xid)) $ do
              rec
                let adjustCronExprNoEnterEvt = keydownExclude Enter updateCronExpr
                let adjustCronNameNoEnterEvt = keydownExclude Enter updateCronName
                let cronExprEnterEvt = keydown Enter updateCronExpr
                let cronNameEnterEvt = keydown Enter updateCronName

                conrExprDyn <- holdDyn M.empty $ leftmost
                                 [ ("bgcolor" =: "yellow") <$ adjustCronExprNoEnterEvt
                                 , M.empty <$ cronExprEnterEvt]
                conrNameDyn <- holdDyn M.empty $ leftmost
                                 [ ("bgcolor" =: "yellow") <$ adjustCronNameNoEnterEvt
                                 , M.empty <$ cronNameEnterEvt]
                let mkUpdateRequest
                      = fmap CronTimerUpdateRequest $ CronTimer
                               <$> value updateCronName
                               <*> value updateCronExpr
                               <*> fmap ce_xid cronDyn

                let mkDeleteRequest
                      = fmap CronTimerDeleteRequest $ fmap (fromJust . ce_xid) cronDyn
                
                selectEvt <- fmap (ffilter id . _checkbox_change) $ el "td" $ checkbox False def
                updateCronExpr <- elDynAttr "td" conrExprDyn $ inputElement $ def 
                  & inputElementConfig_setValue .~ leftmost
                      [ fmap ce_expr cronEvt
                      , tag (fmap ce_expr cronBehavior) pb]
                updateCronName <- elDynAttr "td" conrNameDyn $ inputElement $ def
                  & inputElementConfig_setValue .~ leftmost
                      [ fmap ce_name cronEvt
                      , tag (fmap ce_name cronBehavior)  pb]
                
              return $ mergeList [ tagPromptlyDyn mkUpdateRequest cronExprEnterEvt
                                 , tagPromptlyDyn mkUpdateRequest cronNameEnterEvt
                                 , tagPromptlyDyn mkDeleteRequest selectEvt]
        return $ mergeWith (++) [ fmap (:[]) createEvt 
                                , fmap toList updateDeleteEvt]
    deleteEvt :: Event t [WSRequestMessage] <- el "tfoot" $ el "tr" $ do
      let deleteSelectEvt = filter isCronTimerDeleteRequest <$> cronTimerEvt

      {--
      deleteDyn <- foldDyn (\wsMsgs xs -> foldr wsMsgs $ \msg -> 
                               xs
                              )
                     [] deleteSelectEvt
      --}
      el "th" blank
      deleteEvt :: Event t () <- elAttr "th" ("colspan" =: "2") $ do
                       domEvent Click . fst <$> elClass' "button" "ui small button teal" (text "删除")
      return never
    return $ mergeWith (++) [ filter (not . isCronTimerDeleteRequest) <$> cronTimerEvt
                            , deleteEvt]

