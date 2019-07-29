{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Frontend.Class where

import Prelude
import Reflex.Dom.Core
import Common.Types.DataNetwork
import Common.WebSocketMessage

import qualified Data.Tree as TR
import qualified Data.Text as T
import Control.Monad (forM_)

class ToUI a where
  toLabel :: a -> T.Text
  toIcon :: a -> T.Text
  toDOM :: DomBuilder t m => T.Text -> TR.Tree a -> m ()

instance ToUI DataCircuitPart where
  toLabel (DCIP_EmbededDataCircuit _) = "内嵌子数据电路"
  toLabel (DCIP_EmbededDataConduit _) = "内嵌数据导管"
  toLabel _ = T.empty
  toIcon DCIP_RootBindNode = "angle double down icon"
  toIcon DCIP_RootAlternativeNode = "arrows alternate vertical icon"
  toIcon (DCIP_EmbededDataCircuit _) = "box icon"
  toIcon (DCIP_EmbededDataConduit _) = "box icon"
  toIcon _ = T.empty
  toDOM _ (TR.Node DCIP_RootBindNode xs) =
    divClass "list" $ forM_ xs (toDOM (toIcon DCIP_RootBindNode))
  toDOM _ (TR.Node DCIP_RootAlternativeNode xs) =
    divClass "list" $ forM_ xs (toDOM (toIcon DCIP_RootAlternativeNode))
  toDOM icon (TR.Node v@(DCIP_EmbededDataConduit dataConduit) []) =
    divClass "item" $ do
      elClass "i" icon blank
      divClass "content" $ divClass "header" $ text (toLabel v)
      toDOM (toIcon v) (dcoPartCombinator dataConduit)
  toDOM icon (TR.Node x xs)  = do
    divClass "item" $ do
      elClass "i" icon blank
      divClass "content" $ divClass "header" $ text (toLabel x)
      divClass "list" $ forM_ xs (toDOM (toIcon x))
    
instance ToUI DataConduitPart where
  toLabel (DCOP_EmbededLogicFragment _) = "内嵌逻辑碎片"
  toLabel _ = ""
  toIcon DCOP_RootBindNode = "angle double down icon"
  toIcon DCOP_RootAlternativeNode = "arrows alternate vertical icon"
  toIcon (DCOP_EmbededLogicFragment _) = "box icon"
  toIcon _ = ""
  toDOM _ (TR.Node DCOP_RootBindNode xs) =
    divClass "list" $ forM_ xs (toDOM (toIcon DCOP_RootBindNode))
  toDOM _ (TR.Node DCOP_RootAlternativeNode xs) =
    divClass "list" $ forM_ xs (toDOM (toIcon DCOP_RootAlternativeNode))
  toDOM icon (TR.Node v@(DCOP_EmbededLogicFragment logicFragement) []) =
    divClass "item" $ do
      elClass "i" icon blank
      divClass "content" $ divClass "header" $ text (toLabel v)
      toDOM (toIcon v) (lfPartCombinator logicFragement)
  toDOM icon (TR.Node x xs)  = do
    divClass "item" $ do
      elClass "i" icon blank
      divClass "content" $ divClass "header" $ text (toLabel x)
      divClass "list" $ forM_ xs (toDOM (toIcon x))


instance ToUI LogicFragmentPart where
  toLabel (LFP_EmbededLogicFragment _) = "内嵌子逻辑碎片"
  toLabel _ = ""
  toIcon LFP_RootBindNode = "angle double down icon"
  toIcon LFP_RootAlternativeNode = "arrows alternate vertical icon"
  toIcon (LFP_EmbededLogicFragment _) = "box icon"
  toDOM _ (TR.Node v@LFP_RootBindNode xs) =
    divClass "list" $ forM_ xs (toDOM (toIcon v))
  toDOM _ (TR.Node v@LFP_RootAlternativeNode xs) =
    divClass "list" $ forM_ xs (toDOM (toIcon v))
  toDOM icon (TR.Node v@(LFP_EmbededLogicFragment logicFragement) []) =
    divClass "item" $ do
      elClass "i" icon blank
      divClass "content" $ divClass "header" $ text (toLabel v)
      toDOM (toIcon v) (lfPartCombinator logicFragement)
  toDOM icon (TR.Node x xs)  = do
    divClass "item" $ do
      elClass "i" icon blank
      divClass "content" $ divClass "header" $ text (toLabel x)
      divClass "list" $ forM_ xs (toDOM (toIcon x))
  
