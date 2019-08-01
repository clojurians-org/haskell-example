{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.Types.EventLake where

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

data GlobalEventLake = GlobalEventLake {
    gelCronTimer :: [(Int64, ELCronTimer)]
  , gelFileWatcher :: [(Int64, ELFileWatcher)]
  , gelSQLScanner :: [(Int64, ELSQLScanner)]
  } deriving (Generic, Show)
instance J.ToJSON GlobalEventLake
instance J.FromJSON GlobalEventLake
instance Default GlobalEventLake

data EventLake = EL_CronTimer ELCronTimer
               | EL_FileWatcher ELFileWatcher
               | EL_SQLScanner ELSQLScanner
data ELCronTimer = ELCronTimer {
    elctName :: T.Text
  , elctExpr :: T.Text
  , elctXid :: Maybe Int64
  } deriving (Generic, Show, Eq)
instance J.ToJSON ELCronTimer
instance J.FromJSON ELCronTimer
instance Default ELCronTimer

data ELFileWatcher = ELFileWatcher {
    elfwName :: T.Text
  , elfwXid :: Maybe Int64
  } deriving (Generic, Show, Eq)
instance J.ToJSON ELFileWatcher
instance J.FromJSON ELFileWatcher
instance Default ELFileWatcher

data ELSQLScanner = ELSQLScanner {
    elssName :: T.Text
  , elssXid :: Maybe Int64
  } deriving (Generic, Show, Eq)
instance J.ToJSON ELSQLScanner
instance J.FromJSON ELSQLScanner
instance Default ELSQLScanner
