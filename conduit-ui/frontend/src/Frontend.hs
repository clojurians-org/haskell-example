{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedLabels #-}

module Frontend where

import Common.Api
import Common.WebSocketMessage
import Frontend.Page.DataNetwork.EventPulse (dataNetwork_eventPulse_handle, dataNetwork_eventPulse)
import Frontend.Page.DataNetwork.EffectEngine (dataNetwork_effectEngine_handle, dataNetwork_effectEngine)
import Frontend.Page.DataNetwork.LogicFragement (dataNetwork_logicFragement_handle, dataNetwork_logicFragement)
import Frontend.Page.DataNetwork.DataConduit (dataNetwork_dataConduit_handle, dataNetwork_dataConduit)
import Frontend.Page.DataNetwork.DataCircuit (dataNetwork_dataCircuit_handle, dataNetwork_dataCircuit)
import Frontend.Page.EventSource.CronTimer (eventSource_cronTimer_handle, eventSource_cronTimer)
import Frontend.Page.DataSandbox.DataSource.SQLCursor (dataSource_sqlCursor_handle, dataSource_sqlCursor)

import Prelude

import GHC.Int (Int64)
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

import Control.Monad.Fix (MonadFix)
import Data.Default (def)
import Data.String.Conversions (cs)

import Reflex (never)
import Obelisk.Route.Frontend (RoutedT, RouteToUrl, SetRoute, routeLink, askRoute, subRoute, subRoute_)

import qualified Obelisk.ExecutableConfig as Cfg
import Text.Regex.TDFA ((=~))
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import System.Random (randomRIO)
import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_, readMVar, threadDelay)

import Labels ((:=)(..), Has)

htmlHeader :: DomBuilder t m => m ()
htmlHeader = do
  elAttr "link" ( "rel" =: "stylesheet"
               <> "href" =: "https://cdn.jsdelivr.net/npm/semantic-ui@2.3.3/dist/semantic.min.css") blank

nav :: forall t js m. ( DomBuilder t m, Prerender js m
        , PerformEvent t m, TriggerEvent t m, PostBuild t m
        , RouteToUrl (R FrontendRoute) m, SetRoute t (R FrontendRoute) m) => m ()
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
    elClass "h4" "ui header" $ text "事件源"
    divClass "menu" $ do
      divClass "item" $ routeLink (FrontendRoute_EventSource :/ EventSourceRoute_CronTimer :/ ()) $ text "Cron定时器"      
      divClass "item" $ routeLink (FrontendRoute_EventSource :/ EventSourceRoute_FileWatcher :/ ()) $ text "文件监控器"
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

page :: forall t js m r.
  ( DomBuilder t m, Prerender js m
  , MonadFix m, MonadHold t m
  , PerformEvent t m, TriggerEvent t m, PostBuild t m
  , MonadIO m, MonadIO (Performable m)
  , Has "eventSource_cronTimer" [CronTimer] r
  )
  => MVar r
  -> Event t WSResponseMessage
  -> RoutedT t (R FrontendRoute) m (Event t [WSRequestMessage])
page wsST wsResponseEvt = do
  let wsSTNotUsed = undefined  
  dataNetwork_eventPulse_st <- dataNetwork_eventPulse_handle wsSTNotUsed wsResponseEvt
  dataNetwork_effectEngine_st <- dataNetwork_effectEngine_handle wsST wsResponseEvt
  dataNetwork_logicFragement_st <- dataNetwork_logicFragement_handle wsST wsResponseEvt
  dataNetwork_dataConduit_st <- dataNetwork_dataConduit_handle wsST wsResponseEvt
  dataNetwork_dataCircuit_st <- dataNetwork_dataCircuit_handle wsST wsResponseEvt
  
  eventSource_cronTimer_st <- eventSource_cronTimer_handle wsST wsResponseEvt
  dataSource_sqlCursor_st <- dataSource_sqlCursor_handle wsST wsResponseEvt

  fmap switchDyn $ subRoute $ \case
      FrontendRoute_Main -> text "my main" >> return never
      FrontendRoute_DataNetwork -> fmap switchDyn $ subRoute $ \case
        DataNetworkRoute_EventPulse -> dataNetwork_eventPulse dataNetwork_eventPulse_st
        DataNetworkRoute_EffectEngine -> dataNetwork_effectEngine dataNetwork_effectEngine_st
        DataNetworkRoute_LogicFragement -> dataNetwork_logicFragement dataNetwork_logicFragement_st 
        DataNetworkRoute_DataConduit ->  dataNetwork_dataConduit dataNetwork_dataConduit_st
        DataNetworkRoute_DataCircuit -> dataNetwork_dataCircuit dataNetwork_dataCircuit_st
      FrontendRoute_EventSource -> fmap switchDyn $ subRoute $ \case
        EventSourceRoute_HttpRequest -> text "my EventSourceRoute_HttpRequest" >> return never
        EventSourceRoute_CronTimer -> eventSource_cronTimer eventSource_cronTimer_st 
        EventSourceRoute_FileWatcher -> text "my EventSourceRoute_FileWatcher" >> return never
      FrontendRoute_DataSandbox -> fmap switchDyn $ subRoute $ \case
        DataSandboxRoute_StateContainer -> fmap switchDyn $ subRoute $ \case
          StateContainerRoute_PostgreSQL -> text "my StateContainerRoute_PostgreSQL" >> return never
          StateContainerRoute_RocksDB -> text "my StateContainerRoute_RocksDB" >> return never
          StateContainerRoute_SQLLite -> text "my StateContainerRoute_SQLLite" >> return never
        DataSandboxRoute_DataSource -> fmap switchDyn $ subRoute $ \case
          DataSourceRoute_Kafka -> text "my DataSourceRoute_Kafka" >> return never
          DataSourceRoute_WebSocket -> text "my DataSourceRoute_WebSocket" >> return never
          DataSourceRoute_RestAPI -> text "my DataSourceRoute_RestAPI" >> return never        
          DataSourceRoute_SQLCursor -> dataSource_sqlCursor dataSource_sqlCursor_st
          DataSourceRoute_MinIO -> text "my DataSourceRoute_MinIO" >> return never        
        DataSandboxRoute_DataService -> fmap switchDyn $ subRoute $ \case
          DataServiceRoute_QueryService_PostgREST -> text "my DataServiceRoute_QueryService_PostgREST" >> return never
          DataServiceRoute_QueryService_ElasticSearch -> text "my DataServiceRoute_QueryService_ElasticSearch" >> return never
          DataServiceRoute_QueryService_HBase -> text "my DataServiceRoute_QueryService_HBase" >> return never
          DataServiceRoute_QueryService_Kudu -> text "my DataServiceRoute_QueryService_Kudu" >> return never                
          DataServiceRoute_FileService_MinIO -> text "my DataServiceRoute_FileService_MinIO" >> return never
          DataServiceRoute_FileService_HDFS -> text "my DataServiceRoute_FileService_HDFS" >> return never
          DataServiceRoute_FileService_SFtp -> text "my DataServiceRoute_FileService_SFtp" >> return never
          DataServiceRoute_NotifyService_WebHook -> text "my DataServiceRoute_NotifyService_WebHook" >> return never
          DataServiceRoute_NotifyService_Email -> text "my DataServiceRoute_NotifyService_Email" >> return never

handleWSRequest :: forall t m js.
  ( DomBuilder t m, Prerender js m, MonadHold t m
  , PerformEvent t m, TriggerEvent t m, PostBuild t m)
  => T.Text -> Event t [WSRequestMessage] -> m (Event t WSResponseMessage)
handleWSRequest wsURL wsRequests =
  prerender (return never) $ do
    ws <- webSocket wsURL $
      def & webSocketConfig_send .~ ((fmap . fmap) J.encode wsRequests)
    return $ fmap (fromJust . J.decode . cs) (_webSocket_recv ws)

mkWSStateContainer :: IO (MVar ("eventSource_cronTimer" := [CronTimer]))
mkWSStateContainer = newMVar
  ( #eventSource_cronTimer := [] )

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = htmlHeader
  , _frontend_body = do
      (host, port, path) <- liftIO $ askWSInfo
      let wsURL = "ws://" <> host <> ":" <> (cs . show) port <> path

      wsST <- liftIO mkWSStateContainer
      rec
        wsResponseEvt <- handleWSRequest wsURL wsRequestEvt
        wsRequestEvt <- do
          divClass "ui message icon" $ do
            elClass "i" "notched circle loading icon" blank
            elClass "h1" "ui header" $
              routeLink (FrontendRoute_Main :/ ()) $ text "实时数据中台" 
          divClass "ui grid" $ do
            divClass "ui two wide column vertical menu visible compact" $ nav
            divClass "ui fourteen wide column container" $ page wsST wsResponseEvt
      return ()
  }

