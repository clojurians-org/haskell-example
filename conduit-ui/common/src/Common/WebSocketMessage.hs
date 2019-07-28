{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.WebSocketMessage where

import Prelude
import Common.Types.DataSandbox
import Common.Types.DataNetwork

import GHC.Int (Int64)

import GHC.Generics (Generic)
import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Tree as TR
import Data.Default (Default(def))

import Control.Applicative (liftA2)
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
                    -- CronTimer
                    | CronTimerCreateRequest CronTimer
                    | CronTimerReadRequest Int64
                    | CronTimerUpdateRequest CronTimer
                    | CronTimerDeleteRequest Int64
                    | CronTimerActiveRequest Int64
                    | CronTimerKillRequest Int64
                    -- SQLCursor
                    | SQLCursorCreateRequest SQLCursor_DataSource
                    | SQLCursorReadRequest Int64
                    | SQLCursorUpdateRequest SQLCursor_DataSource
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
                     | SQLCursorCreateResponse (Either String SQLCursor_DataSource)
                     | SQLCursorReadResponse (Either String SQLCursor_DataSource)
                     | SQLCursorUpdateResponse (Either String SQLCursor_DataSource)
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

