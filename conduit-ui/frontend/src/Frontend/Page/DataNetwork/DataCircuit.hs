{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}


module Frontend.Page.DataNetwork.DataCircuit
  (dataNetwork_dataCircuit_handle, dataNetwork_dataCircuit) where

import Common.WebSocketMessage
import Prelude

import Reflex.Dom.Core
import Control.Monad (forM_, void)
import Control.Monad.Fix (MonadFix)

import qualified Data.Text as T
import Data.String.Conversions (cs)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent (MVar, newMVar, putMVar, modifyMVar, modifyMVar_, readMVar, threadDelay)

dataNetwork_dataCircuit_handle
  :: forall t m r.
     ( MonadHold t m, MonadFix m
     , MonadIO m, MonadIO (Performable m), PerformEvent t m)
  => MVar r -> Event t WSResponseMessage -> m (Event t T.Text)
dataNetwork_dataCircuit_handle _ wsResponseEvt = do
  return $ fmap (cs . show) $ ffilter isHaskellCodeRunResponse wsResponseEvt

dataNetwork_dataCircuit
  :: forall t m .
     (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
  => Event t T.Text
  -> m (Event t [WSRequestMessage])
dataNetwork_dataCircuit wsEvt = do
  divClass "ui segment basic" $ do
    divClass "ui message compact" $ do
      elClass "h2" "ui header" $ do
        text "数据电路"
      elClass "ul" "list" $ do
        el "li" $ text "连接数据导管配置强大的业务数据逻辑"
        el "li" $ text "根据配置可构建不同的业务数据系统"
  
  return never
