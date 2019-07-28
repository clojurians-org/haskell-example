{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.Types.DataNetwork where

import Common.Class
import Common.Types.DataSandbox
import Prelude

import GHC.Int (Int64)

import GHC.Generics (Generic)
import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Tree as TR
import Data.Default (Default(def))

import Control.Applicative (liftA2)
import Control.Lens ()

data EventPulse = EventPulse {
    eventPulse_enable :: Bool
  , eventPulse_name :: T.Text
  , eventPulse_desc :: T.Text
  , eventPulse_dataConduits :: [DataCircuitValue]
  }
type DataCircuitId = Int64
data DataCircuitValue = DataCircuitValue {
    dataCircuitValue_enable :: Bool
  , dataCircuitValue_name :: T.Text
  , dataCircuitValue_desc :: T.Text
  , dataCircuitValue_base :: DataCircuitId
  , dataCircuitValue_stateContainers :: [StateContainer]    
  , dataCircuitValue_dataSources :: [DataSource]
  }
data DataCircuit = DataCircuit {
    dataCircuit_name :: T.Text
  , dataCircuit_desc :: T.Text
  , dataCircuit_stateContainers :: [StateContainerHolder]
  , dataCircuit_dataSources :: [DataSourceHolder]
  , dataCircuit_dataServices :: [DataServiceHolder]
  , dataCircuit_subDataCircuits :: [DataCircuit]
  , dataCircuit_dataConduits :: [DataConduit]
  , dataCircuit_partCombinator :: TR.Tree DataCircuitPart
  , dataCircuit_configSchema :: T.Text
  , dataCircuit_requestSchema :: T.Text
  , dataCircuit_responseSchema :: T.Text
  , dataCircuit_xid :: Maybe Int64
  } deriving (Generic, Show, Eq)
instance J.ToJSON DataCircuit
instance J.FromJSON DataCircuit
instance Default DataCircuit
instance ToHaskellCode DataCircuit where
  toHaskellCode dataCircuit = toHaskellCode (dataCircuit_partCombinator dataCircuit)


data DataCircuitPart = DataCircuitPart_RootBindNode
                     | DataCircuitPart_RootAlternativeNode
                     | DataCircuitPart_BindNode T.Text
                     | DataCircuitPart_AlternateNode T.Text
                     | DataCircuitPart_ParallelNode T.Text
                     | DataCircuitPart_LinkedDataCircuit Int64
                     | DataCircuitPart_LinkedDataConduit Int64               
                     | DataCircuitPart_EmbededDataCircuit DataCircuit
                     | DataCircuitPart_EmbededDataConduit DataConduit
  deriving (Generic, Show, Eq)
instance J.ToJSON DataCircuitPart
instance J.FromJSON DataCircuitPart
instance Default DataCircuitPart where def = DataCircuitPart_RootBindNode

instance ToHaskellCode (TR.Tree DataCircuitPart) where
  toHaskellCode (TR.Node DataCircuitPart_RootBindNode xs) = do
    undefined
  
data DataConduit = DataConduit {
    dataConduit_name :: T.Text
  , dataConduit_desc :: T.Text
  , dataConduit_stateContainers :: [StateContainerHolder]
  , dataConduit_dataSources :: [DataSourceHolder]
  , dataConduit_dataServices :: [DataServiceHolder]
  , dataConduit_logicFagements :: [LogicFragment ]
  , dataConduit_partCombinator :: TR.Tree DataConduitPart
  , dataConduit_xid :: Maybe Int64
  } deriving (Generic, Show, Eq)
instance J.ToJSON DataConduit
instance J.FromJSON DataConduit
instance Default DataConduit

data DataConduitPart = DataConduitPart_RootBindNode
                     | DataConduitPart_RootAlternativeNode
                     | DataConduitPart_BindNode T.Text
                     | DataConduitPart_AlternateNode T.Text
                     | DataConduitPart_ParallelNode T.Text
                     | DataConduitPart_LinkedLogicFragment Int64
                     | DataConduitPart_EmbededLogicFragment LogicFragment
  deriving (Generic, Show, Eq)
instance J.ToJSON DataConduitPart
instance J.FromJSON DataConduitPart
instance Default DataConduitPart where def = DataConduitPart_RootBindNode

data LogicFragment = LogicFragment {
    logicFragment_name :: T.Text
  , logicFragment_desc :: T.Text
  , logicFragment_effectEngineCode :: (Maybe EffecteEngine, T.Text)
  , logicFragment_subLogicFragment ::  [LogicFragment ]
  , logicFragment_partCombinator :: TR.Tree LogicFragmentPart
  , logicFragment_xid :: Maybe Int64
  } deriving (Generic, Show, Eq)
instance J.ToJSON LogicFragment
instance J.FromJSON LogicFragment
instance Default LogicFragment

data EffecteEngine = ConduitEngine | JavaEngine | REngine | CEngine
  deriving (Generic, Show, Eq)
instance J.ToJSON EffecteEngine
instance J.FromJSON EffecteEngine
instance Default EffecteEngine where def = ConduitEngine
data LogicFragmentPart = LogicFragmentPart_RootBindNode
                       | LogicFragmentPart_RootAlternativeNode
                       | LogicFragmentPart_BindNode T.Text
                       | LogicFragmentPart_AlternateNode T.Text
                       | LogicFragmentPart_ParallelNode T.Text
                       | LogicFragmentPart_LinkedLogicFragment Int64   
                       | LogicFragmentPart_EmbededLogicFragment LogicFragment
  deriving (Generic, Show, Eq)
instance J.ToJSON LogicFragmentPart
instance J.FromJSON LogicFragmentPart
instance Default LogicFragmentPart where def = LogicFragmentPart_RootBindNode


