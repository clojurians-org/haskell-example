{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend.Page.EventLake.CronTimer
  (eventLake_cronTimer_handle, eventLake_cronTimer) where

import Common.WebSocketMessage
import Common.Types
import Prelude

import Reflex.Dom.Core hiding (mapMaybe)
import Data.Function (on)
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

eventLake_cronTimer_handle
  :: forall t m r.
     ( MonadHold t m, MonadFix m
     , MonadIO m, MonadIO (Performable m), PerformEvent t m )
--      , Has "elCronTimers" [ELCronTimer] r)
  => MVar r -> Event t WSResponseMessage
  -> m (Event t WSResponseMessage, Dynamic t [ELCronTimer])
eventLake_cronTimer_handle wsST wsResponseEvt = do
  let wsEvt = ffilter (isELCronTimerCRES ||| isELCronTimerURES ||| isELCronTimerDRES) wsResponseEvt
  myST <- liftIO $ readMVar wsST
  wsDyn <- foldDyn (\wsMsg xs -> case wsMsg of
                       ELCronTimerCRES (Right cronTimer) -> cronTimer : xs
                       ELCronTimerURES (Right cronTimer) ->
                         cronTimer : filter (on (==) elctXid cronTimer) xs
                       ELCronTimerDRES (Right cronId) ->
                         filter ((/= cronId) . fromJust . elctXid) xs
                       _ -> xs)
             [] wsEvt
{--
  (get #elCronTimers myST) wsEvt
  performEvent $ do
    ffor (updated wsDyn) $ \xs ->  do
      liftIO $ modifyMVar_ wsST $ return . set #elCronTimers xs
--}
      
  return (wsEvt, (fmap (sortOn elctName) wsDyn))

keydownExclude :: (Reflex t, HasDomEvent t e 'KeydownTag, DomEventType e 'KeydownTag ~ Word) => Key -> e -> Event t ()
keydownExclude key = fmapMaybe (\n -> guard $ keyCodeLookup (fromIntegral n) /= key) . domEvent Keydown

eventLake_cronTimer
  :: forall t m.
     ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => (Event t WSResponseMessage, Dynamic t [ELCronTimer])
  -> m (Event t [WSRequestMessage])
eventLake_cronTimer (wsEvt, wsDyn) = do
  -- holdDyn "" (cs . show <$> wsEvt) >>= dynText  
  elClass "table" "ui collapsing table" $ do
    el "thead" $ el "tr" $ do
      el "th" $ checkbox False def
      el "th" $ text "Cron表达式"
      el "th" $ text "事件名称"
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
                  fmap ELCronTimerCREQ $ ELCronTimer
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
            elDynAttr "tr" (ffor cronDyn $ \(ELCronTimer _ _ xid) -> "id" =: (cs . show $ xid)) $ do
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
                      = fmap ELCronTimerUREQ $ ELCronTimer
                               <$> value updateCronName
                               <*> value updateCronExpr
                               <*> fmap elctXid cronDyn

                let mkDeletePayload
                      =  (\case
                              True -> DeleteSelect 
                              False -> DeleteUnselect)
                           <$> value deleteSelect
                           <*> fmap (fromJust . elctXid) cronDyn

                deleteSelect <- el "td" $ checkbox False (def & checkboxConfig_setValue .~ (False <$ wsEvt))
                updateCronExpr <- elDynAttr "td" conrExprDyn $ inputElement $ def 
                  & inputElementConfig_setValue .~ leftmost
                      [ fmap elctExpr cronEvt
                      , tag (fmap elctExpr cronBehavior) pb]
                updateCronName <- elDynAttr "td" conrNameDyn $ inputElement $ def
                  & inputElementConfig_setValue .~ leftmost
                      [ fmap elctName cronEvt
                      , tag (fmap elctName cronBehavior)  pb]

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
                      CronTimerResponseEvent (ELCronTimerDRES _) -> []
                      _ -> xs)
                   xs0 payloads
      deleteDyn <- foldDyn foldPayloads []
                       (mergeWith (++) [ cronTimerPayloadEvt
                                       , (:[]) . CronTimerResponseEvent <$> wsEvt ])

      el "th" blank
      deleteEvt' <- elAttr "th" ("colspan" =: "2") $ do
                         domEvent Click . fst <$> elClass' "button" "ui small button teal" (text "删除")
      -- dynText $ cs . show  <$> deleteDyn
      return (tagPromptlyDyn (fmap ELCronTimerDREQ <$> deleteDyn) deleteEvt')
    return $ mergeWith (++) [ mapMaybe fromCronTimerEvent <$> cronTimerPayloadEvt
                            , deleteEvt ]

