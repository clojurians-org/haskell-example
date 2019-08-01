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
import qualified Data.HashMap.Lazy as M
import Data.Default (Default(def))

import Control.Applicative (liftA2)
import Control.Lens ()
import Data.List (foldl')


data EventPulse = EventPulse {
    epEnable :: Bool
  , epName :: T.Text
  , epDesc :: T.Text
  , epDataCircuitValues :: [DataCircuitValue]
  } deriving (Generic, Show, Eq)
instance J.ToJSON EventPulse
instance J.FromJSON EventPulse
instance Default EventPulse

exampleHaskellCodeBuilderTest :: HaskellCodeBuilder
exampleHaskellCodeBuilderTest = HaskellCodeBuilder
  { hcbCombinators = TR.Node ">>" [ TR.Node "f" []
                                  , TR.Node "<|>"
                                      [ TR.Node "g" []
                                      , TR.Node "h" []]]
  , hcbFns = M.fromList
             [ ("f", "  putStrLn \"hello world\"\n  putStrLn \"???\"")
             , ("g", "  putStrLn \"what's wrong\"")
             , ("h", "  putStrLn \"hi hi hi\"") ] }

instance ToHaskellCodeBuilder EventPulse where
  toHaskellCodeBuilder ep =  do
    let dcivs = (epDataCircuitValues ep)
        hcbCombinators' = do --flip map dataCircuitsValues $ \(DataCircuitValue True name desc ldci ldsa) ->
          -- TR.Node "<|>"
          let label = "keep $ (" <> T.replicate (length dcivs) "," <> ")"
                         <> " <$> " <> T.intercalate " <*> " (fmap (("async " <>) . dcivName) dcivs)
          TR.Node label []
        hcbFns' = (M.unions . fmap (hcbFns . toHaskellCodeBuilder)) dcivs
    HaskellCodeBuilder {hcbCombinators = hcbCombinators', hcbFns = hcbFns'}
  
data DataCircuitValue = DataCircuitValue {
    dcivEnable :: Bool
  , dcivName :: T.Text
  , dcivDesc :: T.Text
  , dcivLinkedDataCircuit :: (Int64, T.Text)  
  , dcivLnkedDataSandbox :: LinkedDataSandbox  
  } deriving (Generic, Show, Eq)
instance J.ToJSON DataCircuitValue
instance J.FromJSON DataCircuitValue
instance Default DataCircuitValue
instance ToHaskellCodeBuilder DataCircuitValue where
  toHaskellCodeBuilder dci = HaskellCodeBuilder
    { hcbCombinators = undefined
    , hcbFns = undefined
    }

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
{--
instance ToHaskellCode DataCircuit where
  toHaskellCode dataCircuit = toHaskellCode (dciPartCombinator dataCircuit)
--}

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

{--
instance ToHaskellCode (TR.Tree DataCircuitPart) where
  toHaskellCode (TR.Node DCIP_RootBindNode xs) = do
    undefined
  toHaskellCode _ = undefined
--}
  
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
