{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Common.Class where

import Prelude

import GHC.Int (Int64)

import GHC.Generics (Generic)
import qualified Data.Aeson as J
import qualified Data.Text as T
import qualified Data.Tree as TR
import Data.Default (Default(def))
import qualified Data.HashMap.Lazy as M

import Control.Applicative (liftA2)
import Control.Lens ()

instance Default Bool where def = True
instance Default T.Text where def = T.empty

data HaskellCodeBuilder = HaskellCodeBuilder {
    hcbCombinators :: TR.Tree T.Text
  , hcbFns :: M.HashMap T.Text T.Text
  } deriving (Generic, Show)

class ToHaskellCodeBuilder a where
  toHaskellCodeBuilder :: a -> HaskellCodeBuilder

toHaskellCode :: HaskellCodeBuilder -> T.Text
toHaskellCode (HaskellCodeBuilder combinators fns) =
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
    
