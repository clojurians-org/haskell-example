{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Common.Class where

import Common.Types
import Common.ExampleData
import Prelude

import GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.Tree as TR
import qualified Data.HashMap.Lazy as M
import Data.Default (Default, def)
import Data.List (foldl')
import Data.String.Conversions (cs)

data HaskellCodeBuilder = HaskellCodeBuilder {
    hcbCombinator :: TR.Tree T.Text
  , hcbFns :: M.HashMap T.Text T.Text
  , hcbSelfFn :: (T.Text, T.Text)
  } deriving (Generic, Show)

instance Default HaskellCodeBuilder

class ToHaskellCodeBuilder a where
  toHaskellCodeBuilder :: a -> HaskellCodeBuilder

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
  toHaskellCodeBuilder ep =  do
    let dcivs@(x:xs) = (epDataCircuitValues ep)
        hcbCombinator' = do
          foldl' (\b a -> TR.Node "<*>" [ b, (hcbCombinator . toHaskellCodeBuilder) a ])
            (TR.Node "<$>" [ TR.Node (T.replicate (length dcivs) ",") []
                           , (hcbCombinator . toHaskellCodeBuilder) x ])
            xs
        hcbFns' = (M.unions . fmap (hcbFns . toHaskellCodeBuilder)) dcivs
    def {hcbCombinator = hcbCombinator', hcbFns = hcbFns'}

instance ToHaskellCodeBuilder DataCircuitValue where
  toHaskellCodeBuilder dci = def
    { hcbCombinator = TR.Node (snd . dcivLinkedDataCircuit $ dci) []
    , hcbFns = undefined
    }

instance ToHaskellCodeBuilder DataCircuit where
  toHaskellCodeBuilder dci = def

instance ToHaskellCodeBuilder DataCircuitPart where
  toHaskellCodeBuilder dci = def

instance ToHaskellCodeBuilder DataConduit where
  toHaskellCodeBuilder dci = def

instance ToHaskellCodeBuilder DataConduitPart where
  toHaskellCodeBuilder dci = def

instance ToHaskellCodeBuilder LogicFragment where
  toHaskellCodeBuilder dci = def

instance ToHaskellCodeBuilder LogicFragmentPart where
  toHaskellCodeBuilder dci = def

instance ToHaskellCodeBuilder DSOSQLCursor where
  toHaskellCodeBuilder dsoSQLCusor = def
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
