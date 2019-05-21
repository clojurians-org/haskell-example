{-# LANGUAGE DeriveGeneric #-}
module Common.Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Time (UTCTime)

data ApiStat = ApiStat
  { _apiStat_success_num :: Int
  , _apiStat_fail_num :: Int
  , _apiStat_ts :: UTCTime
  } deriving (Show, Generic)

instance FromJSON ApiStat
instance ToJSON ApiStat