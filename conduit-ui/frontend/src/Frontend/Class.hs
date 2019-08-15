{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Frontend.Class where

import Common.Types
import Common.WebSocketMessage
import Frontend.Widget
import Prelude
import Reflex.Dom.Core
import Labels

import qualified Data.Tree as TR
import qualified Data.Text as T
import Data.Functor ((<&>))
import Control.Applicative (liftA2)
import Control.Monad (forM_, void, (>=>))
import Control.Monad.Fix (MonadFix)
import Data.Default (Default(def))
import GHC.Int (Int64)

trEB :: forall t m a. (DomBuilder t m) => m a -> m (Event t ())
trEB = fmap (void . domEvent Dblclick . fst) . el' "tr"

trE :: forall t m a. (DomBuilder t m) => m a -> m (Event t ())
trE = trEB

createIcon :: forall t m. (DomBuilder t m) => m ()
createIcon = el "td" $ elClass "i" "paint brush icon" blank

selectE :: forall t m.(DomBuilder t m, PostBuild t m) => m ()
selectE = void $ el "td" $ checkbox False def

tdDynToggle :: forall t m. (DomBuilder t m, PostBuild t m)
  => Dynamic t Bool -> m (Dynamic t Bool)
tdDynToggle bD = el "td" $ divClass "ui toggle checkbox" $ dynCheckboxDB bD <* el "label" blank


tdDynInput :: forall t m. (DomBuilder t m, PostBuild t m)
  => Dynamic t T.Text -> m (Dynamic t T.Text)
tdDynInput = el "td" . divClass "ui mini input" . dynInputDB
tdDyn :: forall t m. (DomBuilder t m, PostBuild t m)
  => Dynamic t T.Text -> m (Dynamic t T.Text)
tdDyn = tdDynInput



class (Default a) => ToTable a where
--  toTR :: a -> m (Behavior t a)
  defDyn :: forall t. Reflex t => Dynamic t a
  defDyn = constDyn def
  toTR :: forall t m. (DomBuilder t m, PostBuild t m) => Dynamic t a -> m ()
  toTable :: forall t m. (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
    => Dynamic t [a] -> m (Event t a)

instance ToTable DSEFSSFtp where
  toTR sftpD = do
    tdDyn (dsefsSFtpName <$> sftpD)
    tdDyn (dsefsSFtpDesc <$> sftpD)
    tdDyn (dsefsSFtpHost <$> sftpD)
    tdDyn (dsefsSFtpFileFormat <$> sftpD)
    tdDyn (dsefsSFtpFilePath <$> sftpD)   
    return ()
  toTable sftpsD = do
    elClass "table" "ui selectable table" $ do
      theadList ["名称", "描述", "主机", "格式", "目的地"]
      el "tbody" $ do
        e0 <- (trE $ createIcon >> toTR (constDyn (def::DSEFSSFtp)))
                <&> tagPromptlyDyn (return def)
        e1 <- switchDyn . fmap leftmost <$> simpleList sftpsD
                (\x -> (trE $ selectE >> toTR x)
                       <&> tagPromptlyDyn x)
        return $ leftmost [e0, e1]
      
    
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
  
