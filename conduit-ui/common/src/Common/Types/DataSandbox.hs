{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.Types.DataSandbox where

import Common.Class
import Prelude

import GHC.Int (Int64)

import GHC.Generics (Generic)
import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Tree as TR
import Data.Default (Default(def))

import Control.Applicative (liftA2)
import Control.Lens ()


data DataSandbox = DataSandbox {
    dataSandbox_stateContainers :: [StateContainer]
  , dataSandbox_dataSources :: [DataSource]
  , dataSandbox_dataServices :: [DataService]
  } deriving (Generic, Show, Eq)
instance J.ToJSON DataSandbox
instance J.FromJSON DataSandbox
instance Default DataSandbox

data StateContainerHolder = StateContainerHolder_PostgreSQL
                          | StateContainerHolder_RocksDB
                          | StateContainerHolder_SQLLite
  deriving (Generic, Show, Eq)
instance J.ToJSON StateContainerHolder
instance J.FromJSON StateContainerHolder

data StateContainer = StateContainer_PostgreSQL PostgreSQL_StateContainer
                    | StateContainer_RocksDB RocksDB_StateContainer
                    | StateContainer_SQLLite SQLLite_StateContainer
  deriving (Generic, Show, Eq)
instance J.ToJSON StateContainer
instance J.FromJSON StateContainer
                    
data PostgreSQL_StateContainer = PostgreSQL_StateContainer
  deriving (Generic, Show, Eq)
instance J.ToJSON PostgreSQL_StateContainer
instance J.FromJSON PostgreSQL_StateContainer
instance Default PostgreSQL_StateContainer

data RocksDB_StateContainer = RocksDB_StateContainer
  deriving (Generic, Show, Eq)
instance J.FromJSON RocksDB_StateContainer
instance J.ToJSON RocksDB_StateContainer
instance Default RocksDB_StateContainer

data SQLLite_StateContainer = SQLLite_StateContainer
  deriving (Generic, Show, Eq)
instance J.FromJSON SQLLite_StateContainer
instance J.ToJSON SQLLite_StateContainer
instance Default SQLLite_StateContainer

data DataSourceHolder = DataSourceHolder_RestAPI
                      | DataSourceHolder_SQLCursor
                      | DataSourceHolder_MinIO
  deriving (Generic, Show, Eq)
instance J.ToJSON DataSourceHolder
instance J.FromJSON DataSourceHolder

data DataSource = DataSource_SQLCursor SQLCursor_DataSource
                | DataSource_MinIO MinIO_DataSource
                | DataSource_RestAPI RestAPI_DataSource
  deriving (Generic, Show, Eq)
instance J.ToJSON DataSource
instance J.FromJSON DataSource

data RestAPI_DataSource = RestAPI_DataSource
  { restAPI_dataSource_name :: T.Text
  , restAPI_dataSource_host :: T.Text }
  deriving (Generic, Show, Eq)
instance J.ToJSON RestAPI_DataSource
instance J.FromJSON RestAPI_DataSource
instance Default RestAPI_DataSource

data SQLCursor_DataSource = SQLCursor_DataSource
  { sqlCursor_dataSource_name :: T.Text
  , sqlCursor_dataSource_type :: T.Text
  , sqlCursor_dataSource_host :: T.Text
  , sqlCursor_dataSource_database :: T.Text
  , sqlCursor_dataSource_username :: T.Text
  , sqlCursor_dataSource_password :: T.Text
  , sqlCursor_dataSource_table :: T.Text
  , sqlCursor_dataSource_fields :: [T.Text]
  , sqlCursor_dataSource_xid :: Maybe Int64 }
  deriving (Generic, Show, Eq)
instance J.ToJSON SQLCursor_DataSource
instance J.FromJSON SQLCursor_DataSource
instance Default SQLCursor_DataSource

data MinIO_DataSource = MinIO_DataSource
  { minIO_dataSource_name :: T.Text
  , minIO_dataSource_xid :: Maybe Int64 }
  deriving (Generic, Show, Eq)
instance J.ToJSON MinIO_DataSource
instance J.FromJSON MinIO_DataSource
instance Default MinIO_DataSource


data DataServiceHolder = DataServiceHolder_QueryService_PostgREST
                     | DataServiceHolder_QueryService_ElasticSearch
                     | DataServiceHolder_QueryService_HBase
                     | DataServiceHolder_QueryService_Kudu
                     | DataServiceHolder_FileService_MinIO
                     | DataServiceHolder_FileService_HDFS
                     | DataServiceHolder_FileService_SFtp
  deriving (Generic, Show, Eq)
instance J.ToJSON DataServiceHolder
instance J.FromJSON DataServiceHolder

data DataService = DataService_QueryService_PostgREST PostgREST_QueryService
                 | DataService_FileService_MinIO MinIO_FileService
  deriving (Generic, Show, Eq)
instance J.ToJSON DataService
instance J.FromJSON DataService

data PostgREST_QueryService = PostgREST_QueryService
  deriving (Generic, Show, Eq)
instance J.ToJSON PostgREST_QueryService
instance J.FromJSON PostgREST_QueryService
instance Default PostgREST_QueryService

data MinIO_FileService = MinIO_FileService
  deriving (Generic, Show, Eq)
instance J.ToJSON MinIO_FileService
instance J.FromJSON MinIO_FileService
instance Default MinIO_FileService