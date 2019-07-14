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
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)

import qualified Data.ByteString as B (ByteString)
import Data.Text (Text)
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

type MyAPI = "api" :> "ping" :> Get '[PlainText] String
myAPI :: Snap String
myAPI = return "pong\n"

wsConduitApp :: WS.ServerApp
wsConduitApp pending= do
  conn <- WS.acceptRequest pending
  runConduit
    $ (forever $ liftIO (WS.receiveData conn) >>= yield)
   .| C.map (id @Text)
   .| C.mapM_ (WS.sendTextData conn)

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

repl :: IO ()
repl = do
  let stmt = unlines [
          "do"
        , "  let sql = [str|select"
        , "                |  id, name, description, 'type'"
        , "                |, state, timeliness, params, result_plugin_type"
        , "                |, vendor_id, server_id, success_code"
        , "                |from tb_interface"
        , "                |] :: B.ByteString"
        , "  let pgSettings = H.settings \"10.132.37.200\" 5432 \"monitor\" \"monitor\" \"monitor\""
        , "  let (curName, cursorSize, chanSize) = (\"larluo\", 200, 1000)"
        , "  let textColumn = HD.column HD.text"
        , "  let mkRow = (,,,,,,,,,,)"
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
        , "  runResourceT $ do"
        , "    chan <- pgToChan connection sql curName cursorSize chanSize mkRow"
        , "    runConduit $"
        , "      (sourceTBMChan chan"
        , "            .| C.concat"
        , "            .| C.take 2"
        , "            .| C.mapM_ (liftIO . print))"
        ]

  r <- I.runInterpreter $ do
    I.loadModules ["backend/src/Fn.hs" ]
    I.setTopLevelModules ["Fn"]
    I.set [I.languageExtensions I.:= [ I.OverloadedStrings
                                     , I.OverloadedLabels
                                     , I.TemplateHaskell
                                     , I.QuasiQuotes]]
    I.runStmt stmt
  print r
