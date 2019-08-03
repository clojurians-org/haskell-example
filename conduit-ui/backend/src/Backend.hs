{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend where

import Fn
import Prelude
import Common.Route
import Obelisk.Backend

import Common.Types
import Common.ExampleData

import Backend.HttpServer (serveHTTP)
import Backend.WebSocketServer (serveWebSocket)

import Data.Maybe (isJust, fromJust)
import Control.Exception (finally)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.List (unlines, lines)

import Data.Proxy (Proxy(..))
import Data.Functor.Identity (Identity(..))
import Data.Dependent.Sum (DSum (..))

import Data.String.Conversions (cs)

import Control.Applicative ((<|>))
import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_, readMVar, threadDelay)

import Text.Regex.TDFA ((=~))

initAppST :: IO (MVar AppST)
initAppST = newMVar exampleFaasCenter

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      appST <- liftIO $ initAppST
      serve $ do
        \case
          BackendRoute_Missing :=> _ -> return ()
          BackendRoute_API :=> _ -> serveHTTP
          BackendRoute_WSConduit :=> _ -> serveWebSocket appST
  , _backend_routeEncoder = backendRouteEncoder
  }

