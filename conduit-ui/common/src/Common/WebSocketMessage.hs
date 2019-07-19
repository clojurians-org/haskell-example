{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Common.WebSocketMessage where

import Prelude

import GHC.Int (Int64)
import GHC.Generics (Generic)
import qualified Data.Aeson as J
import qualified Data.Text as T

data CronEventDef = CronEventDef {
    ce_name :: T.Text
  , ce_expr :: T.Text
  , ce_xid :: Int64
  } deriving (Generic, Show)
instance J.ToJSON CronEventDef

data JobConfig = JobConfig T.Text deriving (Generic, Show)
instance J.ToJSON JobConfig
data ServerST = ServerST {
    ss_cronEventSTs :: [(T.Text, CronEventDef)]
  , ss_jobConfigSTs :: [(T.Text, JobConfig)]
  } deriving (Generic, Show)
instance J.ToJSON ServerST

data AppEvent = CronCreateEvent T.Text
              | CronModifyEvent T.Text
              | CronDeleteEvent T.Text
data AppMessage = AppST ServerST | AppEvent
  deriving (Generic, Show)
instance J.ToJSON AppMessage



