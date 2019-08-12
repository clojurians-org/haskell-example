{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Api where

import Prelude
import qualified Data.Text as T
import Data.FileEmbed (embedFile)
-- import Obelisk.ExecutableConfig.Lookup (getConfigs)

import Data.Maybe (fromJust, maybe)
import Data.Functor ((<&>))
import Control.Applicative ((<|>))
import Data.String.Conversions (cs)
import Data.Bifunctor (bimap, first, second)
import Control.Lens

commonStuff :: String
commonStuff = "Here is a string defined in code common to the frontend and backend."

askWSInfoPure :: (T.Text, Int, T.Text)
askWSInfoPure = do
    let configRoute = cs $(embedFile "config/common/route")
        (host, port) = second (read . cs . T.tail) $ parseHostPort (T.strip configRoute)
    (host, port, "/wsConduit")
  where
    parseHostPort :: T.Text -> (T.Text, T.Text)
    parseHostPort configRoute =
      T.breakOn ":" . fromJust $  T.stripPrefix "https://" configRoute
                            <|> T.stripPrefix "http://" configRoute

askWSInfo :: IO (T.Text, Int, T.Text)
askWSInfo = return askWSInfoPure

apiRepl :: IO ()
apiRepl = askWSInfo >>= print
