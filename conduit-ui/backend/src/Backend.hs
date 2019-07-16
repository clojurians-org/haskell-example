{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE DataKinds #-}
module Backend where

import Prelude
import Common.Route
import Obelisk.Backend

import Fn
import Control.Exception (finally)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)

import qualified Data.ByteString as B (ByteString)
import qualified Data.Text as T
import Data.List (unlines, lines)

import Data.Proxy (Proxy(..))
import Data.Functor.Identity (Identity(..))
import Data.Dependent.Sum (DSum (..))

import Snap.Core (Snap, liftSnap)
import qualified Network.WebSockets as WS
import Network.WebSockets.Snap (runWebSocketsSnap)
import Servant.Server (serveSnap)
import Servant.API ((:>)(..), Get, PlainText)

import qualified Language.Haskell.Interpreter as I
import Data.Conduit (runConduit, yield, (.|))
import qualified Data.Conduit.Combinators as C

import Data.String.Conversions (cs)

type MyAPI = "api" :> "ping" :> Get '[PlainText] String
myAPI :: Snap String
myAPI = return "pong\n"

wsConduitApp :: WS.ServerApp
wsConduitApp pending= do
  conn <- WS.acceptRequest pending
  runConduit
    $ (forever $ liftIO (WS.receiveData conn) >>= yield)
   .| C.map (id @T.Text)
   .| C.mapM (I.runInterpreter . dynHaskell)
   .| C.mapM_ (WS.sendTextData conn . (id @T.Text . cs . show))

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve $ do
      \case
        BackendRoute_Missing :=> _ -> return ()
        BackendRoute_API :=> _ -> do
          liftSnap $ serveSnap (Proxy::Proxy MyAPI) myAPI
        BackendRoute_WSConduit :=> _ -> do
          runWebSocketsSnap wsConduitApp
  , _backend_routeEncoder = backendRouteEncoder
  }

dynHaskell :: T.Text -> I.Interpreter ()
dynHaskell stmt = do
  I.loadModules ["backend/src/Fn.hs" ]
  I.setTopLevelModules ["Fn"]
  I.set [I.languageExtensions I.:= [ I.OverloadedStrings
                                   , I.OverloadedLabels
                                   , I.TemplateHaskell
                                   , I.QuasiQuotes]]
  I.runStmt (cs stmt)
