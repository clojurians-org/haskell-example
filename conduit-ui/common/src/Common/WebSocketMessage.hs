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
data AppST = MkAppST {
    app_cronEventSTs :: [CronEventDef]
  , app_jobConfigSTs :: [JobConfig]
  } deriving (Generic, Show)
instance J.ToJSON AppST

data AppEvent = CronCreateEvent T.Text
              | CronModifyEvent T.Text
              | CronDeleteEvent T.Text
data AppMessage = AppST | AppEvent
  deriving (Generic, Show)
instance J.ToJSON AppMessage



