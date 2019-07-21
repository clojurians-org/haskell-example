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
instance J.FromJSON CronTimerDef

data JobConfig = JobConfig T.Text deriving (Generic, Show)
instance J.ToJSON JobConfig
instance J.FromJSON JobConfig

data AppST = AppST {
    app_cronTimerSTs :: [CronTimerDef]
  , app_jobConfigSTs :: [JobConfig]
  } deriving (Generic, Show)
instance J.ToJSON AppST
instance J.FromJSON AppST

data WSRequestEvent = HaskellCodeRunRequest T.Text
                    | CronTimerCreateRequest CronTimerDef
                    | CronTimerReadRequest Int64
                    | CronTimerUpdateRequest CronTimerDef
                    | CronTimerDeleteRequest Int64
                    | CronTimerActiveRequest Int64
                    | CronTimerKillRequest Int64
  deriving (Generic, Show)
instance J.ToJSON WSRequestEvent
instance J.FromJSON WSRequestEvent

data WSResponseEvent = HaskellCodeRunResponse (Either String ())
                     | CronTimerCreateResponse (Either String CronTimerDef)
                     | CronTimerReadResponse (Either String CronTimerDef)
                     | CronTimerUpdateResponse (Either String CronTimerDef)
                     | CronTimerDeleteResponse (Either String Int64)
                     | CronTimerActiveResponse (Either String Int64)
                     | CronTimerKillResponse (Either String Int64)
  deriving (Generic, Show)
instance J.ToJSON WSResponseEvent
instance J.FromJSON WSResponseEvent

type WSRequestMessage = WSRequestEvent
data WSResponseMessage = WSResponseInit AppST
                       | WSResponseMore WSResponseEvent
                       | WSResponseUnknown WSRequestMessage
  deriving (Generic, Show)                     
instance J.ToJSON WSResponseMessage
instance J.FromJSON WSResponseMessage