{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Common.WebSocketMessage where

import Prelude

import GHC.Int (Int64)

import GHC.Generics (Generic)
import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Tree as TR
import Data.Default (Default(def))

import Control.Applicative (liftA2)
import Control.Lens ()

class ToUI a where
  label :: a -> T.Text
  icon :: a -> T.Text
  
instance Default T.Text where def = T.empty
data DataCircuit = DataCircuit {
    dataCircuit_name :: T.Text
  , dataCircuit_desc :: T.Text
  , dataCircuit_stateContainers :: [StateContainerHolder]
  , dataCircuit_dataSources :: [DataSourceHolder]
  , dataCircuit_dataServices :: [DataServiceHolder]
  , dataCircuit_subDataCircuits :: [DataCircuit]
  , dataCircuit_dataConduits :: [DataConduit]
  , dataCircuit_partCombinator :: TR.Tree DataCircuitPart
  , dataCircuit_configSchema :: T.Text
  , dataCircuit_requestSchema :: T.Text
  , dataCircuit_responseSchema :: T.Text
  , dataCircuit_xid :: Maybe Int64
  } deriving (Generic, Show)
instance J.ToJSON DataCircuit
instance J.FromJSON DataCircuit
instance Default DataCircuit

data DataCircuitPart = DataCircuitPart_RootBindNode
                     | DataCircuitPart_RootAlternativeNode
                     | DataCircuitPart_BindNode T.Text
                     | DataCircuitPart_AlternateNode T.Text
                     | DataCircuitPart_ParallelNode T.Text
                     | DataCircuitPart_LinkedDataCircuit Int64
                     | DataCircuitPart_LinkedDataConduit Int64               
                     | DataCircuitPart_EmbededDataCircuit DataCircuit
                     | DataCircuitPart_EmbededDataConduit DataConduit
  deriving (Generic, Show)
instance J.ToJSON DataCircuitPart
instance J.FromJSON DataCircuitPart
instance Default DataCircuitPart where def = DataCircuitPart_RootBindNode
{--
instance ToUI HaskellCodeNode where
  label (BindNode x) = x
  label (AlternateNode x) = x
  label (ParallelNode x) = x
  label _ = ""
  icon RootBindNode = "angle double down icon"
  icon RootAlternativeNode = "arrows alternate vertical icon"
  icon (BindNode x) = "angle double down icon"
  icon (AlternateNode x) = "arrows alternate vertical icon"
  icon (ParallelNode x) = "code icon"
  icon _ = ""
--}
instance ToUI DataCircuitPart where
  label _ = ""
  icon _ = ""
isDataCircuitPartRootNode :: DataCircuitPart -> Bool
isDataCircuitPartRootNode DataCircuitPart_RootBindNode = True
isDataCircuitPartRootNode DataCircuitPart_RootAlternativeNode = True
isDataCircuitPartRootNode _ = False

data DataConduit = DataConduit {
    dataConduit_name :: T.Text
  , dataConduit_desc :: T.Text
  , dataConduit_stateContainers :: [StateContainerHolder]
  , dataConduit_dataSources :: [DataSourceHolder]
  , dataConduit_dataServices :: [DataServiceHolder]
  , dataConduit_partCombinator :: TR.Tree DataConduitPart
  , dataConduit_xid :: Maybe Int64
  } deriving (Generic, Show)
instance J.ToJSON DataConduit
instance J.FromJSON DataConduit
instance Default DataConduit

data DataConduitPart = DataConduitPart_RootBindNode
                     | DataConduitPart_RootAlternativeNode
                     | DataConduitPart_BindNode T.Text
                     | DataConduitPart_AlternateNode T.Text
                     | DataConduitPart_ParallelNode T.Text
                     | DataConduitPart_LinkedLogicFragment Int64
                     | DataConduitPart_EmbededLogicFragment LogicFragment
  deriving (Generic, Show)
instance J.ToJSON DataConduitPart
instance J.FromJSON DataConduitPart
instance Default DataConduitPart where def = DataConduitPart_RootBindNode

data LogicFragment = LogicFragment {
    logicFragment_name :: T.Text
  , logicFragment_desc :: T.Text
  , logicFragment_effectEngineCode :: (Maybe EffecteEngine, T.Text)
  , logicFragment_partCombinator :: TR.Tree LogicFragmentPart
  , logicFragment_xid :: Maybe Int64
  } deriving (Generic, Show)
instance J.ToJSON LogicFragment
instance J.FromJSON LogicFragment
instance Default LogicFragment

data EffecteEngine = Conduit | Java | R | C
  deriving (Generic, Show)
instance J.ToJSON EffecteEngine
instance J.FromJSON EffecteEngine
instance Default EffecteEngine where def = Conduit
data LogicFragmentPart = LogicFragmentPart_RootBindNode
                       | LogicFragmentPart_RootAlternativeNode
                       | LogicFragmentPart_BindNode T.Text
                       | LogicFragmentPart_AlternateNode T.Text
                       | LogicFragmentPart_ParallelNode T.Text
                       | LogicFragmentPart_LinkedLogicFragment Int64                       
                       | LogicFragmentPart_EmbededLogicFragment LogicFragment
                       | LogicFragmentPart_EffectEngineCode T.Text
  deriving (Generic, Show)
instance J.ToJSON LogicFragmentPart
instance J.FromJSON LogicFragmentPart
instance Default LogicFragmentPart where def = LogicFragmentPart_RootBindNode

data StateContainerHolder = StateContainer_PostgreSQL
                          | StateContainer_RocksDB
                          | StateContainer_SQLLite
  deriving (Generic, Show)                       
instance J.ToJSON StateContainerHolder
instance J.FromJSON StateContainerHolder

data DataSourceHolder = DataSource_RestAPI
                      | DataSource_SQLCursor
                      | DataSource_MinIO
  deriving (Generic, Show)
instance J.ToJSON DataSourceHolder
instance J.FromJSON DataSourceHolder

data DataServiceHolder = DataService_QueryService_PostgREST
                     | DataService_QueryService_ElasticSearch
                     | DataService_QueryService_HBase
                     | DataService_QueryService_Kudu
                     | DataService_FileService_MinIO
                     | DataService_FileService_HDFS
                     | DataService_FileService_SFtp
  deriving (Generic, Show)
instance J.ToJSON DataServiceHolder
instance J.FromJSON DataServiceHolder

data SQLCursor = SQLCursor { sqlCursor_name :: T.Text
                            , sqlCursor_type :: T.Text
                            , sqlCursor_host :: T.Text
                            , sqlCursor_database :: T.Text
                            , sqlCursor_username :: T.Text
                            , sqlCursor_password :: T.Text
                            , sqlCursor_table :: T.Text
                            , sqlCursor_fields :: [T.Text]
                            , sqlCursor_xid :: Maybe Int64 }
  deriving (Generic, Show)
instance J.ToJSON SQLCursor
instance J.FromJSON SQLCursor

data RestAPI = RestAPI { restAPI_name :: T.Text
                       , restAPI_host :: T.Text }
  deriving (Generic, Show)
instance J.ToJSON RestAPI
instance J.FromJSON RestAPI

data DataService = DServiceQueryService QueryService
                 | DServiceFileService FileService
                 | DServiceNotifyService NotifyService
  deriving (Generic, Show)
instance J.ToJSON DataService
instance J.FromJSON DataService

data QueryService = QueryService {}
  deriving (Generic, Show)
instance J.ToJSON QueryService
instance J.FromJSON QueryService
data FileService = FileService {}
  deriving (Generic, Show)
instance J.ToJSON FileService
instance J.FromJSON FileService
data NotifyService = NotifyService {}
  deriving (Generic, Show)
instance J.ToJSON NotifyService
instance J.FromJSON NotifyService

data CronTimer = CronTimer {
    ce_name :: T.Text
  , ce_expr :: T.Text
  , ce_xid :: Maybe Int64
  } deriving (Generic, Show)
instance J.ToJSON CronTimer
instance J.FromJSON CronTimer

isSameCronXID :: CronTimer -> CronTimer -> Bool
isSameCronXID (CronTimer _  _ id1) (CronTimer _  _ id2) = id1 == id2

data AppST = AppST {
    _appST_cronTimers :: [CronTimer]
  } deriving (Generic, Show)
instance J.ToJSON AppST
instance J.FromJSON AppST
instance Default AppST

data WSRequestMessage = HaskellCodeRunRequest T.Text
                    -- CronTimer
                    | CronTimerCreateRequest CronTimer
                    | CronTimerReadRequest Int64
                    | CronTimerUpdateRequest CronTimer
                    | CronTimerDeleteRequest Int64
                    | CronTimerActiveRequest Int64
                    | CronTimerKillRequest Int64
                    -- SQLCursor
                    | SQLCursorCreateRequest SQLCursor
                    | SQLCursorReadRequest Int64
                    | SQLCursorUpdateRequest SQLCursor
                    | SQLCursorDeleteRequest Int64
                    | SQLCursorDatabaseReadRequest T.Text
                    | SQLCursorTableReadRequest T.Text
  deriving (Generic, Show)
instance J.ToJSON WSRequestMessage
instance J.FromJSON WSRequestMessage

data WSResponseMessage = WSInitResponse AppST
                     | HaskellCodeRunResponse (Either String ())
                     -- CronTimer
                     | CronTimerCreateResponse (Either String CronTimer)
                     | CronTimerReadResponse (Either String CronTimer)
                     | CronTimerUpdateResponse (Either String CronTimer)
                     | CronTimerDeleteResponse (Either String Int64)
                     | CronTimerActiveResponse (Either String Int64)
                     | CronTimerKillResponse (Either String Int64)

                     -- SQLCursor
                     | SQLCursorCreateResponse (Either String SQLCursor)
                     | SQLCursorReadResponse (Either String SQLCursor)
                     | SQLCursorUpdateResponse (Either String SQLCursor)
                     | SQLCursorDeleteResponse (Either String Int64)
                     | SQLCursorDatabaseReadResponse (Either String T.Text)
                     | SQLCursorTableReadResponse (Either String T.Text)
                     
                     -- Unkown
                     | WSResponseUnknown WSRequestMessage
  deriving (Generic, Show)
instance J.ToJSON WSResponseMessage
instance J.FromJSON WSResponseMessage

----------------
-- Request
----------------
isCronTimerDeleteRequest :: WSRequestMessage -> Bool
isCronTimerDeleteRequest (CronTimerDeleteRequest  _) = True
isCronTimerDeleteRequest _ = False

----------------
-- Response
----------------
isWSInitResponse :: WSResponseMessage -> Bool
isWSInitResponse (WSInitResponse _) = True
isWSInitResponse _ = False

isHaskellCodeRunResponse :: WSResponseMessage -> Bool
isHaskellCodeRunResponse (HaskellCodeRunResponse  _) = True
isHaskellCodeRunResponse _ = False

-- CronTimer
isCronTimerCreateResponse :: WSResponseMessage -> Bool
isCronTimerCreateResponse (CronTimerCreateResponse  _) = True
isCronTimerCreateResponse _ = False

isCronTimerUpdateResponse :: WSResponseMessage -> Bool
isCronTimerUpdateResponse (CronTimerUpdateResponse  _) = True
isCronTimerUpdateResponse _ = False

isCronTimerDeleteResponse :: WSResponseMessage -> Bool
isCronTimerDeleteResponse (CronTimerDeleteResponse  _) = True
isCronTimerDeleteResponse _ = False

-- SQLCursor
isSQLCursorCreateResponse :: WSResponseMessage -> Bool
isSQLCursorCreateResponse (SQLCursorCreateResponse  _) = True
isSQLCursorCreateResponse _ = False

isSQLCursorUpdateResponse :: WSResponseMessage -> Bool
isSQLCursorUpdateResponse (SQLCursorUpdateResponse  _) = True
isSQLCursorUpdateResponse _ = False

isSQLCursorDeleteResponse :: WSResponseMessage -> Bool
isSQLCursorDeleteResponse (SQLCursorDeleteResponse  _) = True
isSQLCursorDeleteResponse _ = False

isSQLCursorDatabaseReadResponse :: WSResponseMessage -> Bool
isSQLCursorDatabaseReadResponse (SQLCursorDatabaseReadResponse _) = True
isSQLCursorDatabaseReadResponse _ = False

isSQLCursorTableReadResponse :: WSResponseMessage -> Bool
isSQLCursorTableReadResponse (SQLCursorTableReadResponse _) = True
isSQLCursorTableReadResponse _ = False

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) = liftA2 (&&)
infixr 3 &&&

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) = liftA2 (||)
infixr 3 |||

