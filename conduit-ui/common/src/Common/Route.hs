{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE StandaloneDeriving #-}


module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Prelude
import Data.Text (Text)
import Data.Functor.Identity
import Data.Functor.Sum

import Obelisk.Route
import Obelisk.Route.TH

-- makeWrapped ''EventName
data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_WSConduit :: BackendRoute ()
  BackendRoute_API :: BackendRoute (Maybe (R APIRoute))
deriving instance Show (BackendRoute a)

data APIRoute :: * -> * where
  APIRoute_Ping :: APIRoute ()
  APIRoute_Event :: APIRoute Text
deriving instance Show (APIRoute a)

  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_DataNetwork :: FrontendRoute (R DataNetworkRoute)
--  FrontendRoute_EventLake :: FrontendRoute (Maybe (R EventLakeRoute))
  FrontendRoute_EventLake :: FrontendRoute (R EventLakeRoute)
  FrontendRoute_DataSandbox :: FrontendRoute (R DataSandboxRoute)
deriving instance Show (FrontendRoute a)

data DataNetworkRoute :: * -> * where
  DataNetworkRoute_EventPulse :: DataNetworkRoute ()
  DataNetworkRoute_EffectEngine :: DataNetworkRoute ()
  DataNetworkRoute_LogicFragement :: DataNetworkRoute ()
  DataNetworkRoute_DataConduit :: DataNetworkRoute ()
  DataNetworkRoute_DataCircuit :: DataNetworkRoute ()
deriving instance Show (DataNetworkRoute a)

data EventLakeRoute :: * -> * where
  EventLakeRoute_CronTimer :: EventLakeRoute ()
  EventLakeRoute_FileWatcher :: EventLakeRoute ()
deriving instance Show (EventLakeRoute a)

data DataSandboxRoute :: * -> * where
  DataSandboxRoute_DataSource :: DataSandboxRoute (R DataSourceRoute)
  DataSandboxRoute_StateContainer :: DataSandboxRoute (R StateContainerRoute)
  DataSandboxRoute_DataService :: DataSandboxRoute (R DataServiceRoute)
deriving instance Show (DataSandboxRoute a)

data StateContainerRoute :: * -> * where
  StateContainerRoute_PostgreSQL :: StateContainerRoute ()  
  StateContainerRoute_RocksDB :: StateContainerRoute ()
  StateContainerRoute_SQLLite :: StateContainerRoute ()
deriving instance Show (StateContainerRoute a)

data DataSourceRoute :: * -> * where
  DataSourceRoute_Kafka :: DataSourceRoute ()
  DataSourceRoute_WebSocket :: DataSourceRoute ()
  DataSourceRoute_RestAPI :: DataSourceRoute ()  
  DataSourceRoute_SQLCursor :: DataSourceRoute ()
  DataSourceRoute_MinIO :: DataSourceRoute ()
deriving instance Show (DataSourceRoute a)

data DataServiceRoute :: * -> * where
  DataServiceRoute_QueryService_PostgREST :: DataServiceRoute ()
  DataServiceRoute_QueryService_ElasticSearch :: DataServiceRoute ()
  DataServiceRoute_QueryService_HBase :: DataServiceRoute ()
  DataServiceRoute_QueryService_Kudu :: DataServiceRoute ()
  DataServiceRoute_FileService_MinIO :: DataServiceRoute ()
  DataServiceRoute_FileService_HDFS :: DataServiceRoute ()
  DataServiceRoute_FileService_SFtp :: DataServiceRoute ()
  DataServiceRoute_NotifyService_WebHook :: DataServiceRoute ()
  DataServiceRoute_NotifyService_Email :: DataServiceRoute ()
deriving instance Show (DataServiceRoute a)  

backendRouteEncoder
  :: Encoder (Either Text) Identity (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName
backendRouteEncoder = handleEncoder (const (InL BackendRoute_Missing :/ ())) $
  pathComponentEncoder $ \case
    InL backendRoute -> case backendRoute of
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_API -> PathSegment "api" $
        maybeEncoder (unitEncoder mempty) $ pathComponentEncoder $ \case
          APIRoute_Ping -> PathSegment "ping" $ unitEncoder mempty
          APIRoute_Event -> PathSegment "event" $ singlePathSegmentEncoder
      BackendRoute_WSConduit -> PathSegment "wsConduit" $ unitEncoder mempty
    InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
      -- The encoder given to PathEnd determines how to parse query parameters,
      -- in this example, we have none, so we insist on it.
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty
      FrontendRoute_DataNetwork -> PathSegment "dataNetwork" $
        pathComponentEncoder $ \case
          DataNetworkRoute_EventPulse -> PathSegment "eventPulse" $ unitEncoder mempty
          DataNetworkRoute_EffectEngine -> PathSegment "effectEngine" $ unitEncoder mempty
          DataNetworkRoute_LogicFragement -> PathSegment "logicFragement" $ unitEncoder mempty
          DataNetworkRoute_DataConduit -> PathSegment "dataConduit" $ unitEncoder mempty
          DataNetworkRoute_DataCircuit -> PathSegment "dataCircuit" $ unitEncoder mempty
      FrontendRoute_EventLake -> PathSegment "eventSource" $
--        maybeEncoder (unitEncoder mempty) $ pathComponentEncoder $ \case
        pathComponentEncoder $ \case
          EventLakeRoute_CronTimer -> PathSegment "cronTimer" $ unitEncoder mempty
          EventLakeRoute_FileWatcher -> PathSegment "fileWatcher" $ unitEncoder mempty
      FrontendRoute_DataSandbox -> PathSegment "dataSandbox" $
        pathComponentEncoder $ \case
          DataSandboxRoute_StateContainer -> PathSegment "stateContainer" $
            pathComponentEncoder $ \case
              StateContainerRoute_PostgreSQL -> PathSegment "postgreSQL" $ unitEncoder mempty        
              StateContainerRoute_RocksDB -> PathSegment "rocksDB" $ unitEncoder mempty
              StateContainerRoute_SQLLite -> PathSegment "sqlLite" $ unitEncoder mempty
          DataSandboxRoute_DataSource -> PathSegment "dataSource" $
            pathComponentEncoder $ \case
              DataSourceRoute_Kafka -> PathSegment "kafka" $ unitEncoder mempty
              DataSourceRoute_WebSocket -> PathSegment "webSocket" $ unitEncoder mempty
              DataSourceRoute_RestAPI -> PathSegment "restApi" $ unitEncoder mempty          
              DataSourceRoute_SQLCursor -> PathSegment "sqlCursor" $ unitEncoder mempty
              DataSourceRoute_MinIO -> PathSegment "minIO" $ unitEncoder mempty
          DataSandboxRoute_DataService -> PathSegment "dataService" $
            pathComponentEncoder $ \case
              DataServiceRoute_QueryService_PostgREST -> PathSegment "queryService_postgREST" $ unitEncoder mempty
              DataServiceRoute_QueryService_ElasticSearch -> PathSegment "queryService_elasticSearch" $ unitEncoder mempty
              DataServiceRoute_QueryService_HBase -> PathSegment "queryService_hbase" $ unitEncoder mempty
              DataServiceRoute_QueryService_Kudu -> PathSegment "queryService_kudu" $ unitEncoder mempty          
              DataServiceRoute_FileService_MinIO -> PathSegment "fileService_minio" $ unitEncoder mempty
              DataServiceRoute_FileService_HDFS -> PathSegment "fileService_hdfs" $ unitEncoder mempty
              DataServiceRoute_FileService_SFtp -> PathSegment "fileService_sftp" $ unitEncoder mempty
              DataServiceRoute_NotifyService_WebHook -> PathSegment "notifyService_webHook" $ unitEncoder mempty
              DataServiceRoute_NotifyService_Email -> PathSegment "notifyService_email" $ unitEncoder mempty          
        
          
concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''APIRoute
  
  , ''FrontendRoute
  , ''DataNetworkRoute  
  , ''EventLakeRoute
  , ''DataSandboxRoute
  , ''DataSourceRoute
  , ''StateContainerRoute
  , ''DataServiceRoute
  ]
