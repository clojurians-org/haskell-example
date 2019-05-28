{-# LANGUAGE DeriveGeneric #-}
module Common.Types where

import Data.Aeson (FromJSON(..), genericParseJSON, defaultOptions, fieldLabelModifier)
import GHC.Generics (Generic)
import Data.Time (UTCTime)
import Data.Maybe (fromJust)
import Data.List (stripPrefix)

data ApiStat = ApiStat
  { _apiStat_ts :: UTCTime
  , _apiStat_success_num :: Int
  , _apiStat_fail_num :: Int
  } deriving (Show, Generic)

instance FromJSON ApiStat where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = fromJust . stripPrefix "_apiStat_"
    }

data ChannelMsg = ChannelMsg
  { _channelMsg_channel :: String
  , _channelMsg_payload :: String
  } deriving (Show, Generic)
instance FromJSON ChannelMsg where
  parseJSON = genericParseJSON defaultOptions {
      fieldLabelModifier = fromJust . stripPrefix "_channelMsg_"
    }