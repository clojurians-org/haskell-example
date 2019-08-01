{-# LANGUAGE NoImplicitPrelude #-}
module Common.Types.Base where

import Prelude
import qualified Data.Text as T
import qualified Data.HashMap.Lazy as M
import Data.Default (Default(def))

instance Default Bool where def = True
instance Default T.Text where def = T.empty
instance Default (M.HashMap k v) where def = M.empty

