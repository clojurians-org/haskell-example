{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.WebSocketMessage where

import Prelude

import Common.Types
import GHC.Int (Int64)

import GHC.Generics (Generic)
import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Tree as TR
import Data.Default (Default(def))

import Data.String.Conversions (cs)
import Control.Applicative (liftA2)
import Control.Lens
import Data.Bifunctor (bimap, first, second)

import Data.Time.Clock (UTCTime)

data Credential = Credential {
    hostName :: T.Text
  , hostPort :: Int
  , username :: T.Text
  , password :: T.Text
  } deriving (Generic, Show)
instance J.ToJSON Credential
instance J.FromJSON Credential

credential :: T.Text -> T.Text -> T.Text -> Credential
credential host username password = do
  let (hostName, hostPort) = second (read . cs . T.tail) $ T.breakOn ":" host
  Credential hostName hostPort username password

data SFtpEntryType = SFtpFille | SFtpDirectory deriving (Generic, Show)
instance J.ToJSON SFtpEntryType
instance J.FromJSON SFtpEntryType
data SFtpEntry = SFtpEntry {
    sftpEntryName :: T.Text
  , sftpEntryType :: SFtpEntryType
  , sftpEntryCTime :: UTCTime
  } deriving (Generic, Show)
instance J.ToJSON SFtpEntry
instance J.FromJSON SFtpEntry

type SftpPath = TR.Tree SFtpEntry
  
data WSRequestMessage = AppInitREQ 
                    | HaskellCodeRunRequest T.Text
                    -- EventPulse
                    | EventPulseCREQ EventPulse
                    | EventPulseAREQ T.Text
                    | EventPulseKREQ T.Text                    
                    -- CronTimer
                    | ELCronTimerCREQ ELCronTimer
                    | ELCronTimerRREQ Int64
                    | ELCronTimerUREQ ELCronTimer
                    | ELCronTimerDREQ Int64
                    -- SQLCursor
                    | DSOSQLCursorCREQ DSOSQLCursor
                    | DSOSQLCursorRREQ Int64
                    | DSOSQLCursorUREQ DSOSQLCursor
                    | DSOSQLCursorDREQ Int64
                    | DSOSQLCursorDatabaseRREQ T.Text
                    | DSOSQLCursorTableRREQ T.Text
                    -- SFTP
                    | DSEFSSFtpCREQ DSEFSSFtp
                    | DSEFSSFtpFileRREQ Credential (Maybe T.Text)
  deriving (Generic, Show)
instance J.ToJSON WSRequestMessage
instance J.FromJSON WSRequestMessage

data WSResponseMessage = NeverRES
                     | AppInitRES AppST
                     | HaskellCodeRunResponse (Either String ())
                     -- EventPulse
                     | EventPulseCRES (Either String EventPulse)
                     | EventPulseARES (Either String ())
                     | EventPulseKRES (Either String ())                     
                     -- CronTimer
                     | ELCronTimerCRES (Either String ELCronTimer)
                     | ELCronTimerRRES (Either String ELCronTimer)
                     | ELCronTimerURES (Either String ELCronTimer)
                     | ELCronTimerDRES (Either String Int64)

                     -- SQLCursor
                     | DSOSQLCursorCRES (Either String DSOSQLCursor)
                     | DSOSQLCursorRRES (Either String DSOSQLCursor)
                     | DSOSQLCursorURES (Either String DSOSQLCursor)
                     | DSOSQLCursorDRES (Either String Int64)
                     | DSOSQLCursorDatabaseRRES (Either String T.Text)
                     | DSOSQLCursorTableRRES (Either String T.Text)
                     
                     -- Unkown
                     | WSResponseUnknown WSRequestMessage
  deriving (Generic, Show)
instance J.ToJSON WSResponseMessage
instance J.FromJSON WSResponseMessage

----------------
-- Request
----------------
isELCronTimerDREQ :: WSRequestMessage -> Bool
isELCronTimerDREQ (ELCronTimerDREQ  _) = True
isELCronTimerDREQ _ = False

----------------
-- Response
----------------
isAppInitRES :: WSResponseMessage -> Bool
isAppInitRES (AppInitRES _) = True
isAppInitRES _ = False

isHaskellCodeRunResponse :: WSResponseMessage -> Bool
isHaskellCodeRunResponse (HaskellCodeRunResponse  _) = True
isHaskellCodeRunResponse _ = False

-- CronTimer
isELCronTimerCRES :: WSResponseMessage -> Bool
isELCronTimerCRES (ELCronTimerCRES  _) = True
isELCronTimerCRES _ = False

isELCronTimerURES :: WSResponseMessage -> Bool
isELCronTimerURES (ELCronTimerURES  _) = True
isELCronTimerURES _ = False

isELCronTimerDRES :: WSResponseMessage -> Bool
isELCronTimerDRES (ELCronTimerDRES  _) = True
isELCronTimerDRES _ = False

-- SQLCursor
isDSOSQLCursorCRES :: WSResponseMessage -> Bool
isDSOSQLCursorCRES (DSOSQLCursorCRES  _) = True
isDSOSQLCursorCRES _ = False

isDSOSQLCursorURES :: WSResponseMessage -> Bool
isDSOSQLCursorURES (DSOSQLCursorURES  _) = True
isDSOSQLCursorURES _ = False

isDSOSQLCursorDRES :: WSResponseMessage -> Bool
isDSOSQLCursorDRES (DSOSQLCursorDRES  _) = True
isDSOSQLCursorDRES _ = False

isDSOSQLCursorDatabaseRRES :: WSResponseMessage -> Bool
isDSOSQLCursorDatabaseRRES (DSOSQLCursorDatabaseRRES _) = True
isDSOSQLCursorDatabaseRRES _ = False

isDSOSQLCursorTableRRES :: WSResponseMessage -> Bool
isDSOSQLCursorTableRRES (DSOSQLCursorTableRRES _) = True
isDSOSQLCursorTableRRES _ = False

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) = liftA2 (&&)
infixr 3 &&&

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) = liftA2 (||)
infixr 3 |||
