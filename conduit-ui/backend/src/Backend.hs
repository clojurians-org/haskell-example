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
import Data.Maybe (isJust, fromJust)
import Control.Exception (finally)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)

import Data.Either.Combinators (mapLeft)
import qualified Data.ByteString as B
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
import qualified Data.Conduit.List as CL

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

import System.Random (randomRIO)


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

wsHandle :: WSRequestMessage -> IO WSResponseMessage
wsHandle = \case
  HaskellCodeRunRequest r ->
    return . HaskellCodeRunResponse . mapLeft show =<< (I.runInterpreter . dynHaskell) r
  CronTimerCreateRequest (CronTimer name expr Nothing) -> do
    rid <- randomRIO (10, 100)
    return . CronTimerCreateResponse . Right $ CronTimer name expr (Just rid)
  CronTimerUpdateRequest r -> do
    return . CronTimerUpdateResponse . Right $ r
  CronTimerDeleteRequest r -> do
    putStrLn $ "CronTimerDeleteResponse: " ++ (show r)
    return . CronTimerDeleteResponse . Right $ r
  unknown ->
    return . WSResponseUnknown $ unknown

wsConduitApp :: MVar AppST -> WS.ServerApp
wsConduitApp appST pending= do
  putStrLn "websocket connection accepted ..."
  conn <- WS.acceptRequest pending
  readMVar appST >>= WS.sendTextData conn . J.encode . WSInitResponse

  runConduit
    $ (forever $ liftIO (WS.receiveData conn) >>= yield . J.decode)
   .| CL.mapMaybe (id @(Maybe WSRequestMessage))
   .| C.mapM wsHandle
   .| C.mapM_ (WS.sendTextData conn . J.encode)

initAppST :: IO (MVar AppST)
initAppST = do
  newMVar $ AppST
    [ CronTimer "larluo1" "*/5 * * *" (Just 1)
    , CronTimer "larluo2" "*/4 * * *" (Just 2)]
{--  
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
  { _backend_run = \serve -> do
      {-- void . keep  $ do
      chan <- liftIO $ newTBMChanIO 1000
      serverST <- liftIO $ initServerST
      liftIO $ readMVar serverST >>= print
      async (eventGenerator chan) <|> (liftIO $
      --}
      appST <- liftIO $ initAppST
      serve $ do
        \case
          BackendRoute_Missing :=> _ -> return ()
          BackendRoute_API :=> _ -> do
            liftSnap $ serveSnap (Proxy::Proxy MyAPI) myAPI
          BackendRoute_WSConduit :=> _ -> do
            runWebSocketsSnap (wsConduitApp appST)
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
