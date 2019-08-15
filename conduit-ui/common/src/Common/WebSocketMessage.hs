{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

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
import Data.Time.Clock.POSIX (POSIXTime)

import Labels

data TableEntry = TableEntry {
    sqlEntrySchema :: T.Text  
  , sqlEntryName :: T.Text
  , sqlEntrySize :: T.Text
  , sqlEntryCTime :: POSIXTime
  } deriving (Generic, Show)
instance J.ToJSON TableEntry
instance J.FromJSON TableEntry

data SFtpEntryType = SFtpDirectory | SFtpFille | SFtpUnknown deriving (Generic, Show, Eq, Ord)
instance J.ToJSON SFtpEntryType
instance J.FromJSON SFtpEntryType
data SFtpEntry = SFtpEntry {
    sftpEntryName :: T.Text
  , sftpEntryType :: SFtpEntryType
  , sftpEntrySize :: Int
  , sftpEntryCTime :: POSIXTime
  } deriving (Generic, Show)
instance J.ToJSON SFtpEntry
instance J.FromJSON SFtpEntry

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
                    | DSOSQLCursorDatabaseRREQ Credential T.Text T.Text
                    | DSOSQLCursorTableRREQ Credential T.Text T.Text (T.Text, T.Text)
                    -- SFTP
                    | DSEFSSFtpCREQ DSEFSSFtp
                    | DSEFSSFtpDirectoryRREQ Credential (Maybe T.Text)
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
                     | DSOSQLCursorDatabaseRRES (Either String [( "schema" := T.Text
                                                                , "table" := T.Text)])
                     | DSOSQLCursorTableRRES (Either String [( "name" := T.Text
                                                             , "type" := T.Text
                                                             , "desc" := T.Text)])

                    -- SFTP
                     | DSEFSSFtpDirectoryRRES (Either String [SFtpEntry])
                     
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
