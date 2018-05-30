{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( 
    ) where
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Aeson (Value)
import Network.HTTP.Simple (httpLBS, httpJSON, getResponseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)

notFound :: IO Value
notFound = getResponseBody <$> httpJSON "https://www.ele.me/restapi/notfound" :: IO Value
