{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Common.Types
  ( module X, FaaSCenter, AppST, defAppST
  , Credential(..), credential
  ) where

import Common.Types.DataNetwork as X
import Common.Types.DataSandbox as X
import Common.Types.EventLake as X

import Prelude
import GHC.Generics (Generic)
import qualified Data.Text as T
import Data.String.Conversions (cs)
import qualified Data.Aeson as J

import Labels ((:=)(..))
import Labels.JSON ()

import qualified Data.HashMap.Lazy as M
import GHC.Int (Int64)
import Data.Default (Default(def))

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
  let (hostName, xs) = T.breakOn ":" host
      hostPort = if (T.null xs) then 22 else (read . cs . T.tail) xs
  Credential hostName hostPort username password

type FaaSCenter  =
  ( "dataNetwork" := ( "eventPulses" := M.HashMap T.Text X.EventPulse
                     , "dataCircuits" := M.HashMap Int64 X.DataCircuit
                     , "dataConduits" := M.HashMap Int64 X.DataConduit
                     , "logicFragments" := M.HashMap Int64 X.LogicFragment
                     , "primLogicFragments" := M.HashMap Int64 X.PrimLogicFragment)
  , "dataSandbox" := ( "stateContainers" := M.HashMap Int64 X.StateContainer
                     , "dataSources" := M.HashMap Int64 X.DataSource
                     , "dataServices" := M.HashMap Int64 X.DataService )
  , "eventLake" := ( "cronTimers" := M.HashMap Int64 X.ELCronTimer
                   , "fileWatchers" := M.HashMap Int64 X.ELFileWatcher
                   , "sqlScanners" := M.HashMap Int64 X.ELSQLScanner )
    )
type AppST = FaaSCenter

defAppST = ( #dataNetwork := ( #eventPulses := M.empty
                          , #dataCircuits := M.empty
                          , #dataConduits := M.empty
                          , #logicFragments := M.empty
                          , #primLogicFragments := M.empty )
        , #dataSandbox := ( #stateContainers := M.empty
                          , #dataSources := M.empty
                          , #dataServices := M.empty )
        , #eventLake := ( #cronTimers := M.empty
                        , #fileWatchers := M.empty
                        , #sqlScanners := M.empty ) )

