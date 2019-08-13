{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

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
import Control.Lens
import Text.Heredoc (str)
import Data.String.Conversions (cs)
import qualified Data.HashMap.Lazy as M
import Labels

data LinkedDataSandbox = LinkedDataSandbox {
    ldsaStateContainers :: [(Int64, T.Text)]
  , ldsaDataSources :: [(Int64, T.Text)]
  , ldsaDataServices :: [(Int64, T.Text)]
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

_DSO_SQLCursor :: Prism DataSource DataSource DSOSQLCursor DSOSQLCursor
_DSO_SQLCursor = prism DSO_SQLCursor $ \case
    DSO_SQLCursor sqlCursor -> Right sqlCursor
    x -> Left x

getDataSourceId :: DataSource -> Maybe Int64
getDataSourceId (DSO_SQLCursor sqlCursor) =  dsoSQLCursorXid sqlCursor
getDataSourceId _ =  Nothing
getDataSourceName :: DataSource -> T.Text
getDataSourceName (DSO_SQLCursor sqlCursor) =  dsoSQLCursorName sqlCursor
getDataSourceName _ =  ""

data DSORestAPI = DSORestAPI
  { dsoRestAPIName :: T.Text
  , dsoRestAPIHost :: T.Text }
  deriving (Generic, Show, Eq)
instance J.ToJSON DSORestAPI
instance J.FromJSON DSORestAPI
instance Default DSORestAPI

data DSOSQLCursor = DSOSQLCursor
  { dsoSQLCursorName :: T.Text
  , dsoSQLCursorDesc :: T.Text
  , dsoSQLCursorType :: T.Text
  , dsoSQLCursorHost :: T.Text
  , dsoSQLCursorDatabase :: T.Text
  , dsoSQLCursorUsername :: T.Text
  , dsoSQLCursorPassword :: T.Text
  , dsoSQLCursorTable :: T.Text
  , dsoSQLCursorFields :: [T.Text]
  , dsoSQLCursorXid :: Maybe Int64 }
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

_DSE_FileService_SFTP :: Prism DataService DataService DSEFSSFtp DSEFSSFtp
_DSE_FileService_SFTP = prism DSE_FileService_SFTP $ \case
    DSE_FileService_SFTP sftp -> Right sftp
    x -> Left x

getDataServiceId :: DataService -> Maybe Int64
getDataServiceId (DSE_QueryService_PostgREST postgREST) =  dseqsPostgRESTXid postgREST
getDataServiceId (DSE_FileService_SFTP sftp) =  dsefsSFtpXid sftp
getDataServiceId _ =  Nothing

getDataServiceName :: DataService -> T.Text
getDataServiceName (DSE_QueryService_PostgREST postgREST) =  dseqsPostgRESTName postgREST
getDataServiceName (DSE_FileService_SFTP sftp) =  dsefsSFtpName sftp
getDataServiceName _ = T.empty

data DSEQSPostgREST = DSEQSPostgREST {
    dseqsPostgRESTName :: T.Text
  , dseqsPostgRESTDesc :: T.Text
  , dseqsPostgRESTXid :: Maybe Int64
  } deriving (Generic, Show, Eq)
instance J.ToJSON DSEQSPostgREST
instance J.FromJSON DSEQSPostgREST
instance Default DSEQSPostgREST

data DSEFSMinIO = DSEFSMinIO {
    dsefsMinIOName :: T.Text
  , dsefsMinIODesc :: T.Text
  , dsefsMinIOXid :: Maybe Int64 
  } deriving (Generic, Show, Eq)
instance J.ToJSON DSEFSMinIO
instance J.FromJSON DSEFSMinIO
instance Default DSEFSMinIO

{--
type DSEFSSFtp = ( "name" := T.Text
                  , "desc" := T.Text
                  , "host" := T.Text
                  , "username" := T.Text
                  , "password" := T.Text
                  , "fileFormat" := T.Text )
--}

data DSEFSSFtp = DSEFSSFtp
  { dsefsSFtpName :: T.Text
  , dsefsSFtpDesc :: T.Text
  , dsefsSFtpHost :: T.Text
  , dsefsSFtpUsername :: T.Text
  , dsefsSFtpPassword :: T.Text
  , dsefsSFtpFileFormat :: T.Text
  , dsefsSFtpFilePath :: T.Text
  , dsefsSFtpFilePattern :: T.Text  
  , dsefsSFtpXid :: Maybe Int64
    }
  deriving (Generic, Show, Eq)
instance J.ToJSON DSEFSSFtp
instance J.FromJSON DSEFSSFtp
instance Default DSEFSSFtp

