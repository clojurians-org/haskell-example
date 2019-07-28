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
  toLabel (DataCircuitPart_EmbededDataCircuit _) = "内嵌子数据电路"
  toLabel (DataCircuitPart_EmbededDataConduit _) = "内嵌数据导管"
  toLabel _ = T.empty
  toIcon DataCircuitPart_RootBindNode = "angle double down icon"
  toIcon DataCircuitPart_RootAlternativeNode = "arrows alternate vertical icon"
  toIcon (DataCircuitPart_EmbededDataCircuit _) = "box icon"
  toIcon (DataCircuitPart_EmbededDataConduit _) = "box icon"
  toIcon _ = T.empty
  toDOM _ (TR.Node DataCircuitPart_RootBindNode xs) =
    divClass "list" $ forM_ xs (toDOM (toIcon DataCircuitPart_RootBindNode))
  toDOM _ (TR.Node DataCircuitPart_RootAlternativeNode xs) =
    divClass "list" $ forM_ xs (toDOM (toIcon DataCircuitPart_RootAlternativeNode))
  toDOM icon (TR.Node v@(DataCircuitPart_EmbededDataConduit dataConduit) []) =
    divClass "item" $ do
      elClass "i" icon blank
      divClass "content" $ divClass "header" $ text (toLabel v)
      toDOM (toIcon v) (dataConduit_partCombinator dataConduit)
  toDOM icon (TR.Node x xs)  = do
    divClass "item" $ do
      elClass "i" icon blank
      divClass "content" $ divClass "header" $ text (toLabel x)
      divClass "list" $ forM_ xs (toDOM (toIcon x))
    
instance ToUI DataConduitPart where
  toLabel (DataConduitPart_EmbededLogicFragment _) = "内嵌逻辑碎片"
  toLabel _ = ""
  toIcon DataConduitPart_RootBindNode = "angle double down icon"
  toIcon DataConduitPart_RootAlternativeNode = "arrows alternate vertical icon"
  toIcon (DataConduitPart_EmbededLogicFragment _) = "box icon"
  toIcon _ = ""
  toDOM _ (TR.Node DataConduitPart_RootBindNode xs) =
    divClass "list" $ forM_ xs (toDOM (toIcon DataConduitPart_RootBindNode))
  toDOM _ (TR.Node DataConduitPart_RootAlternativeNode xs) =
    divClass "list" $ forM_ xs (toDOM (toIcon DataConduitPart_RootAlternativeNode))
  toDOM icon (TR.Node v@(DataConduitPart_EmbededLogicFragment logicFragement) []) =
    divClass "item" $ do
      elClass "i" icon blank
      divClass "content" $ divClass "header" $ text (toLabel v)
      toDOM (toIcon v) (logicFragment_partCombinator logicFragement)
  toDOM icon (TR.Node x xs)  = do
    divClass "item" $ do
      elClass "i" icon blank
      divClass "content" $ divClass "header" $ text (toLabel x)
      divClass "list" $ forM_ xs (toDOM (toIcon x))


instance ToUI LogicFragmentPart where
  toLabel (LogicFragmentPart_EmbededLogicFragment _) = "内嵌子逻辑碎片"
  toLabel _ = ""
  toIcon LogicFragmentPart_RootBindNode = "angle double down icon"
  toIcon LogicFragmentPart_RootAlternativeNode = "arrows alternate vertical icon"
  toIcon (LogicFragmentPart_EmbededLogicFragment _) = "box icon"
  toDOM _ (TR.Node v@LogicFragmentPart_RootBindNode xs) =
    divClass "list" $ forM_ xs (toDOM (toIcon v))
  toDOM _ (TR.Node v@LogicFragmentPart_RootAlternativeNode xs) =
    divClass "list" $ forM_ xs (toDOM (toIcon v))
  toDOM icon (TR.Node v@(LogicFragmentPart_EmbededLogicFragment logicFragement) []) =
    divClass "item" $ do
      elClass "i" icon blank
      divClass "content" $ divClass "header" $ text (toLabel v)
      toDOM (toIcon v) (logicFragment_partCombinator logicFragement)
  toDOM icon (TR.Node x xs)  = do
    divClass "item" $ do
      elClass "i" icon blank
      divClass "content" $ divClass "header" $ text (toLabel x)
      divClass "list" $ forM_ xs (toDOM (toIcon x))
  
