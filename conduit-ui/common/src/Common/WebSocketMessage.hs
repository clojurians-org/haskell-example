{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Common.WebSocketMessage where

import Prelude

import GHC.Int (Int64)
import GHC.Generics (Generic)
import qualified Data.Aeson as J
import qualified Data.Text as T

data CronTimerDef = CronTimerDef {
    ce_name :: T.Text
  , ce_expr :: T.Text
  , ce_xid :: Maybe Int64
  } deriving (Generic, Show)
instance J.ToJSON CronTimerDef

data JobConfig = JobConfig T.Text deriving (Generic, Show)
instance J.ToJSON JobConfig
data AppST = AppST {
    app_cronTimerSTs :: [CronTimerDef]
  , app_jobConfigSTs :: [JobConfig]
  } deriving (Generic, Show)
instance J.ToJSON AppST

data WSRequestEvent = CronTimerCreateRequest CronTimerDef
                    | CronTimerReadRequest Int64
                    | CronTimerUpdateRequest CronTimerDef
                    | CronTimerDeleteRequest Int64
                    | CronTimerActiveRequest Int64
                    | CronTimerKillRequest Int64
  deriving (Generic, Show)                    
data WSResponseEvent = CronTimerCreateResponse CronTimerDef
                     | CronTimerReadResponse CronTimerDef
                     | CronTimerUpdateResponse CronTimerDef
                     | CronTimerDeleteResponse Int64
                     | CronTimerActiveResponse Int64
                     | CronTimerKillResponse Int64
  deriving (Generic, Show)
type WSRequestMessage = WSRequestEvent
data WSResponseMessage = WSResponseInit AppST
                       | WSResponseMore (Either String WSResponseEvent)
