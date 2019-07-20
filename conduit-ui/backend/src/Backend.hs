{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Backend where

import Prelude
import Common.Route
import Obelisk.Backend

import Fn
import Common.WebSocketMessage 

import GHC.Int (Int64)
import Control.Exception (finally)
import Control.Monad (forever, void)
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

import Control.Applicative ((<|>))
import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_, readMVar, threadDelay)
import Transient.Base (TransIO, keep, async, waitEvents)

import Control.Concurrent.STM.TBMChan (TBMChan, newTBMChanIO, closeTBMChan, writeTBMChan)

import qualified Hasql.Connection as H (Connection, Settings, settings, acquire, settings)
import qualified Hasql.Session as H (Session(..), QueryError, run, statement)
import qualified Hasql.Statement as HS (Statement(..))
import qualified Hasql.Encoders as HE (Params(..), unit)
import qualified Hasql.Decoders as HD
import Text.Heredoc (str)

import qualified Data.Aeson as J
import Text.Regex.TDFA ((=~))

eventGenerator :: TBMChan B.ByteString -> IO ()
eventGenerator chan = do
  threadDelay (1000 * 1000 * 5)
  putStrLn "eventGenerator ..."    
  forever $ do
    putStrLn "forever ..."  
    threadDelay (1000 * 1000 * 60)

type MyAPI = "api" :> "ping" :> Get '[PlainText] String
myAPI :: Snap String
myAPI = return "pong\n"

wsConduitApp :: WS.ServerApp
wsConduitApp pending= do
  putStrLn "websocket connection accepted ..."
  conn <- WS.acceptRequest pending
  runConduit
    $ (forever $ liftIO (WS.receiveData conn) >>= yield)
   .| C.map (id @T.Text)
   .| C.mapM (I.runInterpreter . dynHaskell)
   .| C.mapM_ (WS.sendTextData conn . (id @T.Text . cs . show))

wsConduitV2App :: MVar AppST -> TBMChan B.ByteString -> WS.ServerApp
wsConduitV2App m chan pending = do
  conn <- WS.acceptRequest pending
  st <- readMVar m
  WS.sendTextData conn (J.encode st)
  putStrLn ""

{--
initAppST :: IO (MVar AppST)
initAppST = do
  let pgSettings = H.settings "10.132.37.200" 5432 "monitor" "monitor" "monitor"
  let sql = "select schedule, command, jobid from cron.job"
  let parseCronName :: T.Text -> T.Text
      parseCronName schedule = 
        let (_,_,_, x:_)  :: (String, String, String, [String]) =
              (cs schedule ::String) =~ ("pg_notify\\('.+', +'(.+)'\\)" ::String)
        in cs x
  Right connection <- H.acquire pgSettings
  let mkRow = CronEventDef <$> fmap parseCronName (HD.column HD.text)
                           <*> HD.column HD.text
                           <*> HD.column HD.int8
  Right cronEventSTs <- flip H.run connection $ H.statement () $ HS.Statement sql HE.unit (HD.rowList mkRow) True
  newMVar (MkAppST (fmap ((,) <$> ce_name <*>  id) cronEventSTs) [])
--}

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve ->
      {-- void . keep  $ do
      chan <- liftIO $ newTBMChanIO 1000
      serverST <- liftIO $ initServerST
      liftIO $ readMVar serverST >>= print
      async (eventGenerator chan) <|> (liftIO $
      --}
      serve $ do
        \case
          BackendRoute_Missing :=> _ -> return ()
          BackendRoute_API :=> _ -> do
            liftSnap $ serveSnap (Proxy::Proxy MyAPI) myAPI
          BackendRoute_WSConduit :=> _ -> do
            runWebSocketsSnap wsConduitApp
--          BackendRoute_WSConduitV2 :=> _ -> do
--            runWebSocketsSnap (wsConduitV2App serverST chan)
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
