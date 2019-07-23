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

import Data.List (sortOn, foldl')
import Data.List.NonEmpty (NonEmpty(..), toList)
import Data.Maybe (mapMaybe)
import GHC.Int (Int64)

data DeleteSelectPayload = DeleteSelect Int64 | DeleteUnselect Int64
data CronTimerEventAndPayload a = CronTimerRequestEvent WSRequestMessage
                                | CronTimerResponseEvent WSResponseMessage
                                | CronTimerUnitEvent ()
                                | CronTimerPayload a 

isCronTimerEvent :: CronTimerEventAndPayload a -> Bool
isCronTimerEvent (CronTimerRequestEvent x) = True
isCronTimerEvent _ = False

fromCronTimerEvent :: CronTimerEventAndPayload a -> Maybe WSRequestMessage
fromCronTimerEvent (CronTimerRequestEvent x) = Just x
fromCronTimerEvent _  = Nothing

fromCronTimerPayload :: CronTimerEventAndPayload a -> Maybe a
fromCronTimerPayload (CronTimerPayload x) = Just x
fromCronTimerPayload _  = Nothing

eventSource_cronTimer_handle
  :: forall t m r.
     ( MonadHold t m, MonadFix m
     , MonadIO m, MonadIO (Performable m), PerformEvent t m
     , Has "eventSource_cronTimer" [CronTimer] r)
  => MVar r -> Event t WSResponseMessage
  -> m (Event t WSResponseMessage, Dynamic t [CronTimer])
eventSource_cronTimer_handle wsST wsResponseEvt = do
  let wsEvt = ffilter (isWSInitResponse ||| isCronTimerCreateResponse ||| isCronTimerUpdateResponse ||| isCronTimerDeleteResponse) wsResponseEvt
  myST <- liftIO $ readMVar wsST
  wsDyn <- foldDyn (\wsMsg xs -> case wsMsg of
                       WSInitResponse (AppST cronTimers)  -> cronTimers ++ xs
                       CronTimerCreateResponse (Right cronTimer) -> cronTimer : xs
                       CronTimerUpdateResponse (Right cronTimer) -> cronTimer : filter (not . isSameCronXID cronTimer) xs
                       CronTimerDeleteResponse (Right cronId) -> filter ((/= cronId) . fromJust . ce_xid) xs
                       _ -> xs)
             (get #eventSource_cronTimer myST) wsEvt
  performEvent $ do
    ffor (updated wsDyn) $ \xs ->  do
      liftIO $ modifyMVar_ wsST $ return . set #eventSource_cronTimer xs
      
  return (wsEvt, (fmap (sortOn ce_name) wsDyn))

keydownExclude :: (Reflex t, HasDomEvent t e 'KeydownTag, DomEventType e 'KeydownTag ~ Word) => Key -> e -> Event t ()
keydownExclude key = fmapMaybe (\n -> guard $ keyCodeLookup (fromIntegral n) /= key) . domEvent Keydown

eventSource_cronTimer
  :: forall t m.
     ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => (Event t WSResponseMessage, Dynamic t [CronTimer])
  -> m (Event t [WSRequestMessage])
eventSource_cronTimer (wsEvt, wsDyn) = do
  -- holdDyn "" (cs . show <$> wsEvt) >>= dynText  
  elClass "table" "ui collapsing table" $ do
    el "thead" $ el "tr" $ do
      el "th" $ checkbox False def
      el "th" $ text "Cron表达式"
      el "th" $ text "名称"
    cronTimerPayloadEvt <- 
      el "tbody" $ do
        createEvt <- 
          elClass "tr" "warning" $ do
            el "td" $ elClass "i" "notched circle loading icon" blank
            newCronExpr <- el "td" $ inputElement def
            newCronName <- el "td" $ inputElement def
            
            let cronExprEnterEvt = keypress Enter newCronExpr
            let cronNameEnterEvt = keypress Enter newCronName
            let mkCreateRequest =
                  fmap CronTimerCreateRequest $ CronTimer
                    <$> value newCronName
                    <*> value newCronExpr
                    <*> return Nothing
            
            return $ mergeList [ tagPromptlyDyn (CronTimerRequestEvent <$> mkCreateRequest) cronExprEnterEvt
                               , tagPromptlyDyn (CronTimerRequestEvent <$> mkCreateRequest) cronNameEnterEvt]

        updateAndDeletePayloadEvt  <- 
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
                let deleteSelectEvt = _checkbox_change  deleteSelect

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

                let mkDeletePayload
                      =  (\case
                              True -> DeleteSelect 
                              False -> DeleteUnselect)
                           <$> value deleteSelect
                           <*> fmap (fromJust . ce_xid) cronDyn

                deleteSelect <- el "td" $ checkbox False (def & checkboxConfig_setValue .~ (False <$ wsEvt))
                updateCronExpr <- elDynAttr "td" conrExprDyn $ inputElement $ def 
                  & inputElementConfig_setValue .~ leftmost
                      [ fmap ce_expr cronEvt
                      , tag (fmap ce_expr cronBehavior) pb]
                updateCronName <- elDynAttr "td" conrNameDyn $ inputElement $ def
                  & inputElementConfig_setValue .~ leftmost
                      [ fmap ce_name cronEvt
                      , tag (fmap ce_name cronBehavior)  pb]

                -- dynText $ cs . show . fromJust . ce_xid <$> cronDyn
              return $ mergeList [ tagPromptlyDyn (CronTimerRequestEvent <$> mkUpdateRequest) cronExprEnterEvt
                                 , tagPromptlyDyn (CronTimerRequestEvent <$> mkUpdateRequest) cronNameEnterEvt
                                 , tagPromptlyDyn (CronTimerPayload <$> mkDeletePayload) deleteSelectEvt
                                 ]
        return $ mergeWith (++) [ fmap toList createEvt 
                                , fmap toList updateAndDeletePayloadEvt]
    deleteEvt  <- el "tfoot" $ el "tr" $ do

      let foldPayloads payloads xs0 =
            foldl' (\xs payload -> case payload of
                      CronTimerPayload (DeleteSelect x) -> x : filter (/= x) xs
                      CronTimerPayload (DeleteUnselect x) -> filter (/= x) xs
                      CronTimerResponseEvent (CronTimerDeleteResponse _) -> []
                      _ -> xs)
                   xs0 payloads
      deleteDyn <- foldDyn foldPayloads []
                       (mergeWith (++) [ cronTimerPayloadEvt
                                       , (:[]) . CronTimerResponseEvent <$> wsEvt ])

      el "th" blank
      deleteEvt' <- elAttr "th" ("colspan" =: "2") $ do
                         domEvent Click . fst <$> elClass' "button" "ui small button teal" (text "删除")
      -- dynText $ cs . show  <$> deleteDyn
      return (tagPromptlyDyn (fmap CronTimerDeleteRequest <$> deleteDyn) deleteEvt')
    return $ mergeWith (++) [ mapMaybe fromCronTimerEvent <$> cronTimerPayloadEvt
                            , deleteEvt ]

