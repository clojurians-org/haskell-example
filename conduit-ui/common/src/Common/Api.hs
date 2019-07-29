module Common.Api where

import Prelude
import qualified Data.Text as T
import qualified Obelisk.ExecutableConfig as Cfg

import Data.Maybe (fromJust, maybe)
import Data.Functor ((<&>))
import Control.Applicative ((<|>))
import Data.String.Conversions (cs)
import Data.Bifunctor (bimap, first, second)

commonStuff :: String
commonStuff = "Here is a string defined in code common to the frontend and backend."

askWSInfo :: IO (T.Text, Int, T.Text)
askWSInfo = do
    configRoute <- Cfg.get "config/common/route"
                  <&> maybe "http://localhost:8000" id
    let (host, port) = second (read . cs . T.tail) $ hostPort configRoute
    return (host, port, "/wsConduit")
  where
    hostPort :: T.Text -> (T.Text, T.Text)
    hostPort configRoute =
      T.breakOn ":" . fromJust $  T.stripPrefix "https://" configRoute
                            <|> T.stripPrefix "http://" configRoute
    
      
