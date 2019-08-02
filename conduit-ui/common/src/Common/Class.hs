{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.Class where

import Common.Types
import Common.ExampleData
import Prelude

import GHC.Generics (Generic)
import Data.Function ((&))
import qualified Data.Text as T
import qualified Data.Tree as TR
import qualified Data.HashMap.Lazy as M
import Data.Default (Default, def)
import Data.List (foldl')
import Data.String.Conversions (cs)
import Data.Maybe (fromJust)

import Labels (lens)
import Control.Lens (view, (^.), (.~), at)

data HaskellCodeBuilder = HaskellCodeBuilder {
    hcbCombinator :: TR.Tree T.Text
  , hcbFns :: M.HashMap T.Text T.Text
  , hcbSelfFn :: (T.Text, T.Text)
  } deriving (Generic, Show)

instance Default HaskellCodeBuilder

class ToHaskellCodeBuilder a where
  toHaskellCodeBuilder :: FaaSCenter -> a -> HaskellCodeBuilder

toHaskellCode :: HaskellCodeBuilder -> T.Text
toHaskellCode (HaskellCodeBuilder combinators fns selfFn) =
  "do\n  let\n"
       <> (T.unlines . map ("    " <> ) . T.lines .  T.unlines) (map mkFn (M.toList fns))
       <> "  " <> combinatorsCode
  where
    combinatorsCode =
      TR.foldTree (\x xs -> case null xs of
                             True -> x
                             False -> "(" <> T.intercalate x xs <> ")")
        combinators
    mkFn (name, body) = name <> "=do\n" <> body

exampleHaskellCodeBuilder :: HaskellCodeBuilder
exampleHaskellCodeBuilder = def
  { hcbCombinator = TR.Node ">>" [ TR.Node "f" []
                                  , TR.Node "<|>"
                                      [ TR.Node "g" []
                                      , TR.Node "h" []]]
  , hcbFns = M.fromList
             [ ("f", "  putStrLn \"hello world\"\n  putStrLn \"???\"")
             , ("g", "  putStrLn \"what's wrong\"")
             , ("h", "  putStrLn \"hi hi hi\"") ] }

instance ToHaskellCodeBuilder EventPulse where
  toHaskellCodeBuilder faas ep =  do
    let dcivs@(x:xs) = (epDataCircuitValues ep)
        hcbCombinator' = do
          foldl' (\b a -> TR.Node "<*>" [ b, (hcbCombinator . toHaskellCodeBuilder faas) a ])
            (TR.Node "<$>" [ TR.Node (T.replicate (length dcivs) ",") []
                           , (hcbCombinator . toHaskellCodeBuilder faas) x ])
            xs
        hcbFns' = (M.unions . fmap (hcbFns . toHaskellCodeBuilder faas)) dcivs
    def {hcbCombinator = hcbCombinator', hcbFns = hcbFns'}

instance ToHaskellCodeBuilder DataCircuitValue where
  toHaskellCodeBuilder faas dciv = do
    let dci = faas ^. lens #dataNetwork
                    . lens #dataCircuits
                    . at (fst . dcivLinkedDataCircuit $ dciv)
                    & fromJust
    toHaskellCodeBuilder faas dci

instance ToHaskellCodeBuilder DataCircuit where
  toHaskellCodeBuilder faasCenter dci = toHaskellCodeBuilder faasCenter (dciPartCombinator dci)

instance ToHaskellCodeBuilder (TR.Tree DataCircuitPart) where
  toHaskellCodeBuilder faas (TR.Node DCIP_RootBindNode xs) = def
    { hcbCombinator = TR.Node ">>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node DCIP_RootAlternativeNode xs) = def
    { hcbCombinator = TR.Node "<|>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node (DCIP_BindNode _) xs) = def
    { hcbCombinator = TR.Node ">>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node (DCIP_AlternateNode _) xs) = def
    { hcbCombinator = TR.Node "<|>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node (DCIP_LinkedDataCircuit ldci) []) = do
    let dci = faas ^. lens #dataNetwork
                    . lens #dataCircuits
                    . at (fst ldci)
                    & fromJust
    toHaskellCodeBuilder faas dci
  toHaskellCodeBuilder faas (TR.Node (DCIP_LinkedDataConduit ldco) []) = do
    let dci = faas ^. lens #dataNetwork
                    . lens #dataConduits
                    . at (fst ldco)
                    & fromJust
    toHaskellCodeBuilder faas dci
  toHaskellCodeBuilder faas (TR.Node (DCIP_EmbededDataCircuit dci) []) = do
    toHaskellCodeBuilder faas dci
  toHaskellCodeBuilder faas (TR.Node (DCIP_EmbededDataConduit dco) []) = do
    toHaskellCodeBuilder faas dco
  

instance ToHaskellCodeBuilder DataConduit where
  toHaskellCodeBuilder faasCenter dco = toHaskellCodeBuilder faasCenter (dcoPartCombinator dco)

instance ToHaskellCodeBuilder (TR.Tree DataConduitPart) where
  toHaskellCodeBuilder faas (TR.Node DCOP_RootBindNode xs) = def
    { hcbCombinator = TR.Node ">>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node DCOP_RootAlternativeNode xs) = def
    { hcbCombinator = TR.Node "<|>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node (DCOP_BindNode _) xs) = def
    { hcbCombinator = TR.Node ">>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node (DCOP_AlternateNode _) xs) = def
    { hcbCombinator = TR.Node "<|>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node (DCOP_LinkedLogicFragment llf) []) = do
    let lf = faas ^. lens #dataNetwork
                   . lens #logicFragments
                   . at (fst llf)
                   & fromJust
    toHaskellCodeBuilder faas lf
  toHaskellCodeBuilder faas (TR.Node (DCOP_EmbededLogicFragment lf) []) = do
    toHaskellCodeBuilder faas lf
  toHaskellCodeBuilder faas (TR.Node (DCOP_LinkedPrimLogicFragment lplf) []) = do
    let lpf = faas ^. lens #dataNetwork
                   . lens #primLogicFragments
                   . at (fst lplf)
                   & fromJust
    toHaskellCodeBuilder faas lpf
  toHaskellCodeBuilder faas (TR.Node (DCOP_EmbededPrimLogicFragment plf) []) = do
    toHaskellCodeBuilder faas plf

instance ToHaskellCodeBuilder LogicFragment where
  toHaskellCodeBuilder faasCenter lf = toHaskellCodeBuilder faasCenter (lfPartCombinator lf)  

instance ToHaskellCodeBuilder (TR.Tree LogicFragmentPart) where
  toHaskellCodeBuilder faas (TR.Node LFP_RootBindNode xs) = def
    { hcbCombinator = TR.Node ">>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node LFP_RootAlternativeNode xs) = def
    { hcbCombinator = TR.Node "<|>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node (LFP_BindNode _) xs) = def
    { hcbCombinator = TR.Node ">>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node (LFP_AlternateNode _) xs) = def
    { hcbCombinator = TR.Node "<|>" (map (hcbCombinator . toHaskellCodeBuilder faas) xs)
    , hcbFns = M.unions $ map (hcbFns . toHaskellCodeBuilder faas) xs
      }
  toHaskellCodeBuilder faas (TR.Node (LFP_LinkedLogicFragment llf) []) = do
    let lf = faas ^. lens #dataNetwork
                   . lens #logicFragments
                   . at (fst llf)
                   & fromJust
    toHaskellCodeBuilder faas lf
  toHaskellCodeBuilder faas (TR.Node (LFP_EmbededLogicFragment lf) []) = do
    toHaskellCodeBuilder faas lf

instance ToHaskellCodeBuilder DSOSQLCursor where
  toHaskellCodeBuilder _ dsoSQLCusor = def
    { hcbCombinator = TR.Node "dsoSQLCursorChan" []
    , hcbFns = M.fromList [("dsoSQLCursorChan", (cs . unlines)
        [ "  let"
        , "    sql = [str|select"
        , "                |  id, name, description, 'type'"
        , "                |, state, timeliness, params, result_plugin_type"
        , "                |, vendor_id, server_id, success_code"
        , "                |from tb_interface"
        , "                |] :: B.ByteString"
        , "    pgSettings = H.settings \"10.132.37.200\" 5432 \"monitor\" \"monitor\" \"monitor\""
        , "    (curName, cursorSize, chanSize) = (\"larluo\", 200, 1000)"
        , "    textColumn = HD.column HD.text"
        , "    mkRow = (,,,,,,,,,,)"
        , "                <$> fmap (#id :=) textColumn"
        , "                <*> fmap (#name :=) textColumn"
        , "                <*> fmap (#description :=) textColumn"
        , "                <*> fmap (#type :=) textColumn"
        , "                <*> fmap (#state :=) textColumn"
        , "                <*> fmap (#timeliness :=) textColumn"
        , "                <*> fmap (#params :=) textColumn"
        , "                <*> fmap (#result_plugin_type :=) textColumn"
        , "                <*> fmap (#vendor_id :=) textColumn"
        , "                <*> fmap (#server_id :=) textColumn"
        , "                <*> fmap (#success_code :=) textColumn"
        , "  Right connection <- liftIO $ H.acquire pgSettings"
        , "  pgToChan connection sql curName cursorSize chanSize mkRow"
          ] )]
      }
