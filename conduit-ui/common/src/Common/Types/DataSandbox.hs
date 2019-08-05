{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.Types.DataSandbox where

import Common.Types.Base
import Prelude

import GHC.Int (Int64)

import GHC.Generics (Generic)
import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Tree as TR
import Data.Default (Default(def))

import Control.Applicative (liftA2)
import Control.Lens ()
import Text.Heredoc (str)
import Data.String.Conversions (cs)
import qualified Data.HashMap.Lazy as M

data LinkedDataSandbox = LinkedDataSandbox {
    ldsaStateContainers :: [(Int64, StateContainer)]
  , ldsaDataSources :: [(Int64, DataSource)]
  , ldsaDataServices :: [(Int64, DataService)]
  } deriving (Generic, Show, Eq)
instance J.ToJSON LinkedDataSandbox
instance J.FromJSON LinkedDataSandbox
instance Default LinkedDataSandbox

data DataSandboxHolder = DataSandboxHolder {
    dsahStateContainers :: [StateContainerHolder]
  , dsahDataSources :: [DataSourceHolder]
  , dsahDataServices :: [DataServiceHolder]
  } deriving (Generic, Show, Eq)
instance J.ToJSON DataSandboxHolder
instance J.FromJSON DataSandboxHolder
instance Default DataSandboxHolder

data StateContainerHolder = SCH_PostgreSQL
                          | SCH_RocksDB
                          | SCH_SQLLite
  deriving (Generic, Show, Eq)
instance J.ToJSON StateContainerHolder
instance J.FromJSON StateContainerHolder

data StateContainer = SC_PostgreSQL SCPostgreSQL
                    | SC_RocksDB SCRocksDB
                    | SC_SQLLite SCSQLLite
  deriving (Generic, Show, Eq)
instance J.ToJSON StateContainer
instance J.FromJSON StateContainer
                    
data SCPostgreSQL = SCPostgreSQL
  deriving (Generic, Show, Eq)
instance J.ToJSON SCPostgreSQL
instance J.FromJSON SCPostgreSQL
instance Default SCPostgreSQL

data SCRocksDB = SCRocksDB
  deriving (Generic, Show, Eq)
instance J.FromJSON SCRocksDB
instance J.ToJSON SCRocksDB
instance Default SCRocksDB

data SCSQLLite = SCSQLLite
  deriving (Generic, Show, Eq)
instance J.FromJSON SCSQLLite
instance J.ToJSON SCSQLLite
instance Default SCSQLLite

data DataSourceHolder = DSOH_RestAPI
                      | DSOH_SQLCursor
                      | DSOH_MinIO
  deriving (Generic, Show, Eq)
instance J.ToJSON DataSourceHolder
instance J.FromJSON DataSourceHolder

data DataSource = DSO_SQLCursor DSOSQLCursor
                | DSO_MinIO DSOMinIO
                | DSO_RestAPI DSORestAPI
  deriving (Generic, Show, Eq)
instance J.ToJSON DataSource
instance J.FromJSON DataSource

data DSORestAPI = DSORestAPI
  { dsoRestAPIName :: T.Text
  , dsoRestAPIHost :: T.Text }
  deriving (Generic, Show, Eq)
instance J.ToJSON DSORestAPI
instance J.FromJSON DSORestAPI
instance Default DSORestAPI

data DSOSQLCursor = DSOSQLCursor
  { dsoSQLCursorName :: T.Text
  , dsoSQLCursorType :: T.Text
  , dsoSQLCursorHost :: T.Text
  , dsoSQLCursorDatabase :: T.Text
  , dsoSQLCursorUsername :: T.Text
  , dsoSQLCursorPassword :: T.Text
  , dsoSQLCursorTable :: T.Text
  , dsoSQLCursorFields :: [T.Text]
  , dsoSQLXid :: Maybe Int64 }
  deriving (Generic, Show, Eq)
instance J.ToJSON DSOSQLCursor
instance J.FromJSON DSOSQLCursor
instance Default DSOSQLCursor

data DSOMinIO = DSOMinIO
  { dsoMinioName :: T.Text
  , dsoMinioXid :: Maybe Int64 }
  deriving (Generic, Show, Eq)
instance J.ToJSON DSOMinIO
instance J.FromJSON DSOMinIO
instance Default DSOMinIO


data DataServiceHolder = DSEH_QueryService_PostgREST
                     | DSEH_QueryService_ElasticSearch
                     | DSEH_QueryService_HBase
                     | DSEH_QueryService_Kudu
                     | DSEH_FileService_MinIO
                     | DSEH_FileService_HDFS
                     | DSEH_FileService_Sftp
  deriving (Generic, Show, Eq)
instance J.ToJSON DataServiceHolder
instance J.FromJSON DataServiceHolder

data DataService = DSE_QueryService_PostgREST DSEQSPostgREST
                 | DSE_FileService_MinIO DSEFSMinIO
                 | DSE_FileService_SFTP DSEFSSFtp
  deriving (Generic, Show, Eq)
instance J.ToJSON DataService
instance J.FromJSON DataService

data DSEQSPostgREST = DSEQSPostgREST
  deriving (Generic, Show, Eq)
instance J.ToJSON DSEQSPostgREST
instance J.FromJSON DSEQSPostgREST
instance Default DSEQSPostgREST

data DSEFSMinIO = DSEFSMinIO
  deriving (Generic, Show, Eq)
instance J.ToJSON DSEFSMinIO
instance J.FromJSON DSEFSMinIO
instance Default DSEFSMinIO

data DSEFSSFtp = DSEFSSFtp
  { dsefsSFtpName :: T.Text
  , dsefsSFtpDesc :: T.Text
  , dsefsSFtpHost :: T.Text
  , dsefsSFtpUsername :: T.Text
  , dsefsSFtpPassword :: T.Text
  , dsefsSFtpFilePath :: T.Text
  , dsefsSFtpXid :: Maybe Int64
    }
  deriving (Generic, Show, Eq)
instance J.ToJSON DSEFSSFtp
instance J.FromJSON DSEFSSFtp
instance Default DSEFSSFtp
