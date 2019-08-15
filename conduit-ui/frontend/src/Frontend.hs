{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedLabels #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase, MultiParamTypeClasses, PatternSynonyms, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell, TypeFamilies, UndecidableInstances #-}


module Frontend where

import Common.Api
import Common.Types
import Common.WebSocketMessage
import Frontend.FrontendStateT

import Frontend.Page.DataNetwork.EventPulse (dataNetwork_eventPulse)
import Frontend.Page.DataNetwork.EffectEngine (dataNetwork_effectEngine_handle, dataNetwork_effectEngine)
import Frontend.Page.DataNetwork.LogicFragement (dataNetwork_logicFragement_handle, dataNetwork_logicFragement)
import Frontend.Page.DataNetwork.DataConduit (dataNetwork_dataConduit_handle, dataNetwork_dataConduit)
import Frontend.Page.DataNetwork.DataCircuit (dataNetwork_dataCircuit)
import Frontend.Page.EventLake.CronTimer (eventLake_cronTimer_handle, eventLake_cronTimer)
import Frontend.Page.DataSandbox (dataService_sftp, dataSource_sqlCursor)

import Prelude

import GHC.Int (Int64)
import Data.Coerce (coerce)
import Control.Monad (void)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as J
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core

import Control.Monad.IO.Class (MonadIO, liftIO)

import Common.Api
import Common.Route
import Obelisk.Generated.Static

import Data.Functor ((<&>))
import Control.Monad.Fix (MonadFix)
import Data.Default (def)
import Data.Semigroup (Endo(..))
import Data.String.Conversions (cs)

import Reflex
import Obelisk.Route.Frontend
--  ( RoutedT, RouteToUrl(..), SetRoute(..)
--  , mapRoutedT, routeLink, askRoute, subRoute, subRoute_)

import Text.Regex.TDFA ((=~))
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import System.Random (randomRIO)
import Control.Concurrent
  (MVar, newMVar, swapMVar, threadDelay)

import Labels ((:=)(..), Has)

import Control.Monad.Trans (MonadTrans(lift), MonadIO(liftIO))
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Reflex.Host.Class (MonadReflexCreateTrigger)

nav :: forall t js m. ( DomBuilder t m, RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m)
  => m ()
nav = do
  divClass "item" $ do
    elClass "h4" "ui header" $ text "数据网络"
    divClass "menu" $ do
      divClass "item" $ routeLink (FrontendRoute_DataNetwork :/ DataNetworkRoute_EventPulse :/ ()) $ text "事件脉冲"
      divClass "item" $ routeLink (FrontendRoute_DataNetwork :/ DataNetworkRoute_DataCircuit :/ ()) $ text "数据电路"
      divClass "item" $ routeLink (FrontendRoute_DataNetwork :/ DataNetworkRoute_DataConduit :/ ()) $ text "数据导管"
      divClass "item" $ routeLink (FrontendRoute_DataNetwork :/ DataNetworkRoute_LogicFragement :/ ()) $ text "逻辑碎片"      
      divClass "item" $ routeLink (FrontendRoute_DataNetwork :/ DataNetworkRoute_EffectEngine :/ ()) $ text "实效引擎"
      
  divClass "item" $ do
    elClass "h4" "ui header" $ text "事件湖"
    divClass "menu" $ do
      divClass "item" $ routeLink (FrontendRoute_EventLake :/ EventLakeRoute_CronTimer :/ ()) $ text "Cron定时器"      
      divClass "item" $ routeLink (FrontendRoute_EventLake :/ EventLakeRoute_FileWatcher :/ ()) $ text "文件监控器"
      divClass "item" $ text "SQL扫描器"

  divClass "item" $ do
    elClass "h4" "ui header" $ text "状态容器"
    divClass "menu" $ do
      divClass "item" $ routeLink (FrontendRoute_DataSandbox :/ DataSandboxRoute_StateContainer :/ StateContainerRoute_PostgreSQL :/ ()) $ text "PostgreSQL"
      divClass "item" $ routeLink (FrontendRoute_DataSandbox :/ DataSandboxRoute_StateContainer :/ StateContainerRoute_RocksDB :/ ()) $ text "RocksDB"
      divClass "item" $ routeLink (FrontendRoute_DataSandbox :/ DataSandboxRoute_StateContainer :/ StateContainerRoute_SQLLite :/ ()) $ text "SQLLite"

  divClass "item" $ do
    elClass "h4" "ui header" $ text "数据源"
    divClass "menu" $ do
      divClass "item" $ routeLink (FrontendRoute_DataSandbox :/ DataSandboxRoute_DataSource :/ DataSourceRoute_Kafka :/ ()) $ text "Kafka"
      divClass "item" $ routeLink (FrontendRoute_DataSandbox :/ DataSandboxRoute_DataSource :/ DataSourceRoute_WebSocket :/ ()) $ text "WebSocket"
      divClass "item" $ routeLink (FrontendRoute_DataSandbox :/ DataSandboxRoute_DataSource :/ DataSourceRoute_RestAPI :/ ()) $ text "RestAPI"
      divClass "item" $ routeLink (FrontendRoute_DataSandbox :/ DataSandboxRoute_DataSource :/ DataSourceRoute_SQLCursor :/ ()) $ text "SQL游标"
      divClass "item" $ routeLink (FrontendRoute_DataSandbox :/ DataSandboxRoute_DataSource :/ DataSourceRoute_MinIO :/ ()) $ text "MinIO"

  divClass "item" $ do
    elClass "h4" "ui header" $ text "查询服务"
    divClass "menu" $ do
      divClass "item" $ routeLink (FrontendRoute_DataSandbox :/ DataSandboxRoute_DataService:/ DataServiceRoute_QueryService_PostgREST :/ ()) $
        text "PostgREST"
      divClass "item" $ routeLink (FrontendRoute_DataSandbox :/ DataSandboxRoute_DataService:/ DataServiceRoute_QueryService_ElasticSearch :/ ()) $
        text "ElasticSearch"
      divClass "item" $ routeLink (FrontendRoute_DataSandbox :/ DataSandboxRoute_DataService:/ DataServiceRoute_QueryService_PostgREST :/ ()) $
        text "Hbase"
      divClass "item" $ routeLink (FrontendRoute_DataSandbox :/ DataSandboxRoute_DataService:/ DataServiceRoute_QueryService_PostgREST :/ ()) $
        text "Kudu"

  divClass "item" $ do
    elClass "h4" "ui header" $ text "文件服务"
    divClass "menu" $ do
      divClass "item" $ routeLink (FrontendRoute_DataSandbox :/ DataSandboxRoute_DataService:/ DataServiceRoute_FileService_MinIO :/ ()) $
        text "Minio"
      divClass "item" $ routeLink (FrontendRoute_DataSandbox :/ DataSandboxRoute_DataService:/ DataServiceRoute_FileService_HDFS :/ ()) $
        text "HDFS"
      divClass "item" $ routeLink (FrontendRoute_DataSandbox :/ DataSandboxRoute_DataService:/ DataServiceRoute_FileService_SFtp :/ ()) $
        text "Ftp/SFtp"

  divClass "item" $ do
    elClass "h4" "ui header" $ text "通知服务"
    divClass "menu" $ do
      divClass "item" $ routeLink (FrontendRoute_DataSandbox :/ DataSandboxRoute_DataService:/ DataServiceRoute_NotifyService_WebHook :/ ()) $
        text "WebHook"
      divClass "item" $ routeLink (FrontendRoute_DataSandbox :/ DataSandboxRoute_DataService:/ DataServiceRoute_NotifyService_Email :/ ()) $
        text "Email"

  divClass "item" $ do
    elClass "h4" "ui header" $ text "报表服务"
    divClass "menu" $ do
      divClass "item" $ text "公共报表"
      divClass "item" $ text "个人报表"
      divClass "item" $ text "报表开发器"
  
frontend :: Frontend (R FrontendRoute)
frontend = Frontend htmlHeader htmlBody

htmlHeader :: DomBuilder t m => m ()
htmlHeader = do
  elAttr "link" ( "rel" =: "stylesheet"
               <> "href" =: "https://cdn.jsdelivr.net/npm/semantic-ui@2.3.3/dist/semantic.min.css") blank

type AppState t m = EventWriterT t [WSRequestMessage] (FrontendStateT t (FaaSCenter, WSResponseMessage) m)
  
htmlBody :: forall js t m. ObeliskWidget js t (R FrontendRoute) m
  => RoutedT t (R FrontendRoute) m ()
htmlBody =  do
  let (hostname, port, path)  = askWSInfoPure
      wsURL = "ws://" <> hostname <> ":" <> (cs . show $ port) <> path
  mapRoutedT (unraverlAppState wsURL) $ do
    divClass "ui message icon" $ do
      elClass "i" "notched circle loading icon" $ blank
      elClass "h1" "ui header" $
        routeLink (FrontendRoute_Main :/ ()) $ text "实时数据中台"
    
    divClass "ui grid" $ do
      divClass "ui two wide column vertical menu visible compact" $ nav
      divClass "ui fourteen wide column container" page
    return ()
  where
      unraverlAppState :: T.Text -> AppState t m () -> m ()
      unraverlAppState wsURL m = mdo
        appD <- foldDyn appEndo (defAppST, NeverRES) (updateGlobal <$> wsRES)
        wsREQ <- flip runFrontendStateT appD $ runEventWriterT m <&> snd
        wsRES <- handleWSRequest wsURL wsREQ      
        pure ()
    
handleWSRequest :: forall t m js.
  ( DomBuilder t m, Prerender js t m, MonadHold t m
  , PerformEvent t m, TriggerEvent t m, PostBuild t m)
  => T.Text -> Event t [WSRequestMessage] -> m (Event t WSResponseMessage)
handleWSRequest wsURL wsRequests = do
  fmap switchDyn . prerender (return never) $ do
    pbE <- getPostBuild
    ws <- webSocket wsURL $
      def & webSocketConfig_send .~ ((fmap . fmap) J.encode (leftmost [wsRequests, [AppInitREQ] <$ pbE]))
    return $ fmap (fromJust . J.decode . cs) (_webSocket_recv ws)

updateGlobal :: WSResponseMessage -> Endo (FaaSCenter, WSResponseMessage)
updateGlobal = \case
  msg@(AppInitRES state0) -> Endo $ const (state0, msg)
  msg@(DSEFSSFtpDirectoryRRES _) -> Endo $ \(stat, oldMsg) -> (stat, msg)
  msg@(DSOSQLCursorDatabaseRRES _) -> Endo $ \(stat, oldMsg) -> (stat, msg)
  msg@(DSOSQLCursorTableRRES _) -> Endo $ \(stat, oldMsg) -> (stat, msg)  
  _ -> mempty

page :: forall t js m.
  ( DomBuilder t m --, Prerender js m
  , MonadFix m, MonadHold t m
  , PerformEvent t m, TriggerEvent t m, PostBuild t m
  , HasFrontendState t (FaaSCenter, WSResponseMessage) m
  , EventWriter t [WSRequestMessage] m
  )
  => RoutedT t (R FrontendRoute) m ()
page = do
  subRoute_ $ \case
      FrontendRoute_Main -> text "my main"
      FrontendRoute_DataNetwork -> subRoute_ $ \case
        DataNetworkRoute_EventPulse -> dataNetwork_eventPulse
        DataNetworkRoute_DataCircuit -> dataNetwork_dataCircuit
        DataNetworkRoute_DataConduit ->  void $ dataNetwork_dataConduit undefined
        DataNetworkRoute_LogicFragement -> void $ dataNetwork_logicFragement undefined        
        DataNetworkRoute_EffectEngine -> void $ dataNetwork_effectEngine undefined

      FrontendRoute_EventLake -> subRoute_ $ \case
        EventLakeRoute_CronTimer -> void $ eventLake_cronTimer undefined
        EventLakeRoute_FileWatcher -> text "my EventLakeRoute_FileWatcher"
      FrontendRoute_DataSandbox -> subRoute_ $ \case
        DataSandboxRoute_StateContainer -> subRoute_ $ \case
          StateContainerRoute_PostgreSQL -> text "my StateContainerRoute_PostgreSQL"
          StateContainerRoute_RocksDB -> text "my StateContainerRoute_RocksDB"
          StateContainerRoute_SQLLite -> text "my StateContainerRoute_SQLLite"
        DataSandboxRoute_DataSource -> subRoute_ $ \case
          DataSourceRoute_Kafka -> text "my DataSourceRoute_Kafka"
          DataSourceRoute_WebSocket -> text "my DataSourceRoute_WebSocket"
          DataSourceRoute_RestAPI -> text "my DataSourceRoute_RestAPI"
          DataSourceRoute_SQLCursor -> dataSource_sqlCursor
          DataSourceRoute_MinIO -> text "my DataSourceRoute_MinIO"

        DataSandboxRoute_DataService -> subRoute_ $ \case
          DataServiceRoute_QueryService_PostgREST -> text "my DataServiceRoute_QueryService_PostgREST"
          DataServiceRoute_QueryService_ElasticSearch -> text "my DataServiceRoute_QueryService_ElasticSearch"
          DataServiceRoute_QueryService_HBase -> text "my DataServiceRoute_QueryService_HBase"
          DataServiceRoute_QueryService_Kudu -> text "my DataServiceRoute_QueryService_Kudu"
          DataServiceRoute_FileService_MinIO -> text "my DataServiceRoute_FileService_MinIO"
          DataServiceRoute_FileService_HDFS -> text "my DataServiceRoute_FileService_HDFS"
          DataServiceRoute_FileService_SFtp -> dataService_sftp
          DataServiceRoute_NotifyService_WebHook -> text "my DataServiceRoute_NotifyService_WebHook"
          DataServiceRoute_NotifyService_Email -> text "my DataServiceRoute_NotifyService_Email"
