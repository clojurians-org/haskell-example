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

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_WSConduit :: BackendRoute ()
  BackendRoute_API :: BackendRoute (Maybe (R APIRoute))
deriving instance Show (BackendRoute a)

data APIRoute :: * -> * where
  APIRoute_Ping :: APIRoute ()
deriving instance Show (APIRoute a)

  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Main :: FrontendRoute ()
  FrontendRoute_DataNetwork :: FrontendRoute (R DataNetworkRoute)
--  FrontendRoute_EventSource :: FrontendRoute (Maybe (R EventSourceRoute))
  FrontendRoute_EventSource :: FrontendRoute (R EventSourceRoute)
--  FrontendRoute_DataSource :: FrontendRoute (Maybe (R DataSourceRoute))
  FrontendRoute_DataSource :: FrontendRoute (R DataSourceRoute)
--  FrontendRoute_StateContainer :: FrontendRoute (Maybe (R StateContainerRoute))
  FrontendRoute_StateContainer :: FrontendRoute (R StateContainerRoute)
  FrontendRoute_QueryService :: FrontendRoute (R QueryServiceRoute)  
  FrontendRoute_FileService :: FrontendRoute (R FileServiceRoute)
  FrontendRoute_NotifyService :: FrontendRoute (R NotifyServiceRoute)
deriving instance Show (FrontendRoute a)

data DataNetworkRoute :: * -> * where
  DataNetworkRoute_OneClickRun :: DataNetworkRoute ()
  DataNetworkRoute_LogicFragement :: DataNetworkRoute ()
  DataNetworkRoute_DataConduit :: DataNetworkRoute ()
  DataNetworkRoute_DataCircuit :: DataNetworkRoute ()
deriving instance Show (DataNetworkRoute a)

data EventSourceRoute :: * -> * where
  EventSourceRoute_HttpRequest :: EventSourceRoute ()
  EventSourceRoute_CronTimer :: EventSourceRoute ()
  EventSourceRoute_FileWatcher :: EventSourceRoute ()
deriving instance Show (EventSourceRoute a)

data DataSourceRoute :: * -> * where
  DataSourceRoute_Kafka :: DataSourceRoute ()
  DataSourceRoute_WebSocket :: DataSourceRoute ()
  DataSourceRoute_RestAPI :: DataSourceRoute ()  
  DataSourceRoute_SQLCursor :: DataSourceRoute ()
  DataSourceRoute_Minio :: DataSourceRoute ()
deriving instance Show (DataSourceRoute a)

data StateContainerRoute :: * -> * where
  StateContainerRoute_PostgreSQL :: StateContainerRoute ()  
  StateContainerRoute_RocksDB :: StateContainerRoute ()
  StateContainerRoute_SQLLite :: StateContainerRoute ()
deriving instance Show (StateContainerRoute a)

data QueryServiceRoute :: * -> * where
  QueryServiceRoute_PostgREST :: QueryServiceRoute ()
  QueryServiceRoute_ElasticSearch :: QueryServiceRoute ()
  QueryServiceRoute_HBase :: QueryServiceRoute ()
  QueryServiceRoute_Kudu :: QueryServiceRoute ()  
deriving instance Show (QueryServiceRoute a)  
  
data FileServiceRoute :: * -> * where
  FileServiceRoute_MinIO :: FileServiceRoute ()
  FileServiceRoute_HDFS :: FileServiceRoute ()
  FileServiceRoute_SFtp :: FileServiceRoute ()

data NotifyServiceRoute :: * -> * where
  NotifyServiceRoute_WebHook :: NotifyServiceRoute ()
  NotifyServiceRoute_Email :: NotifyServiceRoute ()
  
backendRouteEncoder
  :: Encoder (Either Text) Identity (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName
backendRouteEncoder = handleEncoder (const (InL BackendRoute_Missing :/ ())) $
  pathComponentEncoder $ \case
    InL backendRoute -> case backendRoute of
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_API -> PathSegment "api" $
        maybeEncoder (unitEncoder mempty) $ pathComponentEncoder $ \case
          APIRoute_Ping -> PathSegment "ping" $ unitEncoder mempty
      BackendRoute_WSConduit -> PathSegment "wsConduit" $ unitEncoder mempty
    InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
      -- The encoder given to PathEnd determines how to parse query parameters,
      -- in this example, we have none, so we insist on it.
      FrontendRoute_Main -> PathEnd $ unitEncoder mempty
      FrontendRoute_DataNetwork -> PathSegment "dataNetwork" $
        pathComponentEncoder $ \case
          DataNetworkRoute_OneClickRun -> PathSegment "oneClickRun" $ unitEncoder mempty
          DataNetworkRoute_LogicFragement -> PathSegment "logicFragement" $ unitEncoder mempty
          DataNetworkRoute_DataConduit -> PathSegment "dataConduit" $ unitEncoder mempty
          DataNetworkRoute_DataCircuit -> PathSegment "dataCircuit" $ unitEncoder mempty
      FrontendRoute_EventSource -> PathSegment "eventSource" $
--        maybeEncoder (unitEncoder mempty) $ pathComponentEncoder $ \case
        pathComponentEncoder $ \case
          EventSourceRoute_HttpRequest -> PathSegment "httpRequest" $ unitEncoder mempty          
          EventSourceRoute_CronTimer -> PathSegment "cronTimer" $ unitEncoder mempty
          EventSourceRoute_FileWatcher -> PathSegment "fileWatcher" $ unitEncoder mempty
      FrontendRoute_DataSource -> PathSegment "dataSource" $
--        maybeEncoder (unitEncoder mempty) $ pathComponentEncoder $ \case
        pathComponentEncoder $ \case
          DataSourceRoute_Kafka -> PathSegment "kafka" $ unitEncoder mempty
          DataSourceRoute_WebSocket -> PathSegment "webSocket" $ unitEncoder mempty
          DataSourceRoute_RestAPI -> PathSegment "restApi" $ unitEncoder mempty          
          DataSourceRoute_SQLCursor -> PathSegment "sqlCursor" $ unitEncoder mempty
          DataSourceRoute_Minio -> PathSegment "minio" $ unitEncoder mempty
      FrontendRoute_StateContainer -> PathSegment "stateContainer" $
--        maybeEncoder (unitEncoder mempty) $ pathComponentEncoder $ \case
        pathComponentEncoder $ \case
          StateContainerRoute_PostgreSQL -> PathSegment "postgreSQL" $ unitEncoder mempty        
          StateContainerRoute_RocksDB -> PathSegment "rocksDB" $ unitEncoder mempty
          StateContainerRoute_SQLLite -> PathSegment "sqlLite" $ unitEncoder mempty
      FrontendRoute_QueryService -> PathSegment "queryService" $
        pathComponentEncoder $ \case
          QueryServiceRoute_PostgREST -> PathSegment "postgREST" $ unitEncoder mempty
          QueryServiceRoute_ElasticSearch -> PathSegment "elasticSearch" $ unitEncoder mempty
          QueryServiceRoute_HBase -> PathSegment "hbase" $ unitEncoder mempty
          QueryServiceRoute_Kudu -> PathSegment "kudu" $ unitEncoder mempty          
      FrontendRoute_FileService -> PathSegment "fileService" $
        pathComponentEncoder $ \case
          FileServiceRoute_MinIO -> PathSegment "minio" $ unitEncoder mempty
          FileServiceRoute_HDFS -> PathSegment "hdfs" $ unitEncoder mempty
          FileServiceRoute_SFtp -> PathSegment "sftp" $ unitEncoder mempty
      FrontendRoute_NotifyService -> PathSegment "notifyService" $
        pathComponentEncoder $ \case
          NotifyServiceRoute_WebHook -> PathSegment "webHook" $ unitEncoder mempty
          NotifyServiceRoute_Email -> PathSegment "email" $ unitEncoder mempty          
        
          
concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''APIRoute
  
  , ''FrontendRoute
  , ''DataNetworkRoute  
  , ''EventSourceRoute
  , ''DataSourceRoute
  , ''StateContainerRoute
  , ''QueryServiceRoute
  , ''FileServiceRoute
  , ''NotifyServiceRoute
  ]
