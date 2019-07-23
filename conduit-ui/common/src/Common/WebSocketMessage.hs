{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Common.WebSocketMessage where

import Prelude

import GHC.Int (Int64)
import GHC.Generics (Generic)
import qualified Data.Aeson as J
import qualified Data.Text as T
import Data.Default (Default(def))

import Control.Lens ()

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
                    | CronTimerCreateRequest CronTimer
                    | CronTimerReadRequest Int64
                    | CronTimerUpdateRequest CronTimer
                    | CronTimerDeleteRequest Int64
                    | CronTimerActiveRequest Int64
                    | CronTimerKillRequest Int64
  deriving (Generic, Show)
instance J.ToJSON WSRequestMessage
instance J.FromJSON WSRequestMessage

data WSResponseMessage = WSInitResponse AppST
                     | HaskellCodeRunResponse (Either String ())
                     | CronTimerCreateResponse (Either String CronTimer)
                     | CronTimerReadResponse (Either String CronTimer)
                     | CronTimerUpdateResponse (Either String CronTimer)
                     | CronTimerDeleteResponse (Either String Int64)
                     | CronTimerActiveResponse (Either String Int64)
                     | CronTimerKillResponse (Either String Int64)
                     | WSResponseUnknown WSRequestMessage
  deriving (Generic, Show)
instance J.ToJSON WSResponseMessage
instance J.FromJSON WSResponseMessage

isCronTimerDeleteRequest :: WSRequestMessage -> Bool
isCronTimerDeleteRequest (CronTimerDeleteRequest  _) = True
isCronTimerDeleteRequest _ = False

isWSInitResponse :: WSResponseMessage -> Bool
isWSInitResponse (WSInitResponse _) = True
isWSInitResponse _ = False

isHaskellCodeRunResponse :: WSResponseMessage -> Bool
isHaskellCodeRunResponse (HaskellCodeRunResponse  _) = True
isHaskellCodeRunResponse _ = False


isCronTimerCreateResponse :: WSResponseMessage -> Bool
isCronTimerCreateResponse (CronTimerCreateResponse  _) = True
isCronTimerCreateResponse _ = False

isCronTimerUpdateResponse :: WSResponseMessage -> Bool
isCronTimerUpdateResponse (CronTimerUpdateResponse  _) = True
isCronTimerUpdateResponse _ = False

isCronTimerDeleteResponse :: WSResponseMessage -> Bool
isCronTimerDeleteResponse (CronTimerDeleteResponse  _) = True
isCronTimerDeleteResponse _ = False


isCronTimerResponse :: WSResponseMessage -> Bool
isCronTimerResponse x = isCronTimerCreateResponse x
                     || isCronTimerUpdateResponse x
                     || isCronTimerDeleteResponse x

combine :: (b -> c -> d) -> (a -> b) -> (a -> c) -> (a -> d)
combine op f g = \x -> f x `op` g x

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) = combine (&&)
infixr 3 &&&

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) = combine (||)
infixr 3 |||

