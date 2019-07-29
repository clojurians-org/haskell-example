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
import Common.Types.DataNetwork
import Common.Types.DataSandbox
import Common.Types.EventLake

import Backend.HttpServer (serveHTTP)

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
import GHC.Int (Int64)

import GHC.Generics (Generic)


wsHandle :: WSRequestMessage -> IO WSResponseMessage
wsHandle = \case
  HaskellCodeRunRequest r ->
    return . HaskellCodeRunResponse . mapLeft show =<<
      (I.runInterpreter . dynHaskell) r
  EventPulseAREQ name ->
    return . EventPulseARES . mapLeft show =<<
      (I.runInterpreter . dynHaskell) "putStrLn \"hello world\""
  ELCronTimerCREQ (ELCronTimer name expr Nothing) -> do
    rid <- randomRIO (10, 100)
    return . ELCronTimerCRES . Right $ ELCronTimer name expr (Just rid)
  ELCronTimerUREQ r -> do
    putStrLn $ "ELCronTimerURES: " ++ (show r)    
    return . ELCronTimerURES . Right $ r
  ELCronTimerDREQ r -> do
    putStrLn $ "ELCronTimerDRES: " ++ (show r)
    return . ELCronTimerDRES . Right $ r
  unknown -> do
    putStrLn $ "CronTimerDeleteResponse: " ++ (show unknown)    
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
  newMVar $ def
    { appSTEventLake =  def
        { gelCronTimer = 
            [ (1, ELCronTimer "larluo1" "*/5 * * *" (Just 1))
            , (2, ELCronTimer "larluo2" "*/4 * * *" (Just 2)) ]
          }
      }
    

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      appST <- liftIO $ initAppST
      serve $ do
        \case
          BackendRoute_Missing :=> _ -> return ()
          BackendRoute_API :=> _ -> liftSnap serveHTTP
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
