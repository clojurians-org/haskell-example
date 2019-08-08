{-# LANGUAGE OverloadedStrings #-}
module Common.Api where

import Prelude
import qualified Data.Text as T
import Obelisk.ExecutableConfig.Lookup (getConfigs)

import Data.Maybe (fromJust, maybe)
import Data.Functor ((<&>))
import Control.Applicative ((<|>))
import Data.String.Conversions (cs)
import Data.Bifunctor (bimap, first, second)
import Control.Lens

commonStuff :: String
commonStuff = "Here is a string defined in code common to the frontend and backend."

askWSInfo :: IO (T.Text, Int, T.Text)
askWSInfo = do
    cfgs <- getConfigs
    let configRoute = maybe "http://localhost:8000" cs (cfgs ^. at "common/route")
    let (host, port) = second (read . cs . T.tail) $ parseHostPort (T.strip configRoute)
    return (host, port, "/wsConduit")
  where
    parseHostPort :: T.Text -> (T.Text, T.Text)
    parseHostPort configRoute =
      T.breakOn ":" . fromJust $  T.stripPrefix "https://" configRoute
                            <|> T.stripPrefix "http://" configRoute
    
