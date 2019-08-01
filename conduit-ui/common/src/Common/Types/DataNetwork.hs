{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.Types.DataNetwork where

import Common.Types.Base
import Common.Types.DataSandbox
import Prelude

import GHC.Int (Int64)

import GHC.Generics (Generic)
import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Tree as TR
import qualified Data.HashMap.Lazy as M
import Data.Default (Default(def))

import Control.Applicative (liftA2)
import Control.Lens ()


data EventPulse = EventPulse {
    epEnable :: Bool
  , epName :: T.Text
  , epDesc :: T.Text
  , epDataCircuitValues :: [DataCircuitValue]
  } deriving (Generic, Show, Eq)
instance J.ToJSON EventPulse
instance J.FromJSON EventPulse
instance Default EventPulse

data DataCircuitValue = DataCircuitValue {
    dcivEnable :: Bool
  , dcivName :: T.Text
  , dcivDesc :: T.Text
  , dcivLinkedDataCircuit :: (Int64, T.Text)  
  , dcivLinkedDataSandbox :: LinkedDataSandbox  
  } deriving (Generic, Show, Eq)
instance J.ToJSON DataCircuitValue
instance J.FromJSON DataCircuitValue
instance Default DataCircuitValue

data DataCircuit = DataCircuit {
    dciName :: T.Text
  , dciDesc :: T.Text
  , dciDataSandboxHolder :: DataSandboxHolder
  , dciLinkedDataCircuits :: [DataCircuit]
  , dciDataConduits :: [DataConduit]
  , dciPartCombinator :: TR.Tree DataCircuitPart
  , dciConfigSchema :: T.Text
  , dciRequestSchema :: T.Text
  , dciResponseSchema :: T.Text
  , dciXid :: Maybe Int64
  } deriving (Generic, Show, Eq)
instance J.ToJSON DataCircuit
instance J.FromJSON DataCircuit
instance Default DataCircuit

data DataCircuitPart = DCIP_RootBindNode
                     | DCIP_RootAlternativeNode
                     | DCIP_BindNode T.Text
                     | DCIP_AlternateNode T.Text
                     | DCIP_ParallelNode T.Text
                     | DCIP_LinkedDataCircuit (Int64, T.Text)
                     | DCIP_LinkedDataConduit (Int64, T.Text)
                     | DCIP_EmbededDataCircuit DataCircuit
                     | DCIP_EmbededDataConduit DataConduit
  deriving (Generic, Show, Eq)
instance J.ToJSON DataCircuitPart
instance J.FromJSON DataCircuitPart
instance Default DataCircuitPart where def = DCIP_RootBindNode

data DataConduit = DataConduit {
    dcoName :: T.Text
  , dcoDesc :: T.Text
  , dcoStateContainers :: [StateContainerHolder]
  , dcoDataSources :: [DataSourceHolder]
  , dcoDataServices :: [DataServiceHolder]
  , dcoLogicFagements :: [ LogicFragment ]
  , dcoPartCombinator :: TR.Tree DataConduitPart
  , dcoXid :: Maybe Int64
  } deriving (Generic, Show, Eq)
instance J.ToJSON DataConduit
instance J.FromJSON DataConduit
instance Default DataConduit

data DataConduitPart = DCOP_RootBindNode
                     | DCOP_RootAlternativeNode
                     | DCOP_BindNode T.Text
                     | DCOP_AlternateNode T.Text
                     | DCOP_ParallelNode T.Text
                     | DCOP_LinkedLogicFragment (Int64, T.Text)
                     | DCOP_EmbededLogicFragment LogicFragment
  deriving (Generic, Show, Eq)
instance J.ToJSON DataConduitPart
instance J.FromJSON DataConduitPart
instance Default DataConduitPart where def = DCOP_RootBindNode

data LogicFragment = LogicFragment {
    lfName :: T.Text
  , lfDesc :: T.Text
  , lfEffectEngineCode :: (Maybe EffecteEngine, T.Text)
  , lfLinkedLogicFragments ::  [LogicFragment]
  , lfPartCombinator :: TR.Tree LogicFragmentPart
  , lfXid :: Maybe Int64
  } deriving (Generic, Show, Eq)
instance J.ToJSON LogicFragment
instance J.FromJSON LogicFragment
instance Default LogicFragment

data EffecteEngine = EE_Conduit | EE_Java | EE_R | EE_C
  deriving (Generic, Show, Eq)
instance J.ToJSON EffecteEngine
instance J.FromJSON EffecteEngine
instance Default EffecteEngine where def = EE_Conduit
data LogicFragmentPart = LFP_RootBindNode
                       | LFP_RootAlternativeNode
                       | LFP_BindNode T.Text
                       | LFP_AlternateNode T.Text
                       | LFP_ParallelNode T.Text
                       | LFP_LinkedLogicFragment (Int64, T.Text)
                       | LFP_EmbededLogicFragment LogicFragment
  deriving (Generic, Show, Eq)
instance J.ToJSON LogicFragmentPart
instance J.FromJSON LogicFragmentPart
instance Default LogicFragmentPart where def = LFP_RootBindNode
