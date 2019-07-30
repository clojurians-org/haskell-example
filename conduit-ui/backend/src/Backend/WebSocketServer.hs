{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE LambdaCase #-}

module Backend.WebSocketServer (serveWebSocket) where

import Common.Types
import Common.WebSocketMessage
import Prelude

import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either.Combinators (mapLeft, maybeToLeft, maybeToRight)
import System.Random (randomRIO)

import GHC.Int (Int64)
import Data.String (IsString(..))
import qualified Data.Text as T
import Data.String.Conversions (cs)
import qualified Data.Aeson as J
import qualified Network.WebSockets as WS
import Data.Conduit (runConduit, yield, (.|))
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as CL
import qualified Language.Haskell.Interpreter as I

import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_, readMVar, threadDelay)
import Snap.Core (MonadSnap)
import Network.WebSockets.Snap (runWebSocketsSnap)

import Control.Lens ((^.), (.~), view, over, at)
import Labels (lens)

import Control.Monad.Trans.Except (runExceptT, ExceptT(..), except)
-- import Control.Monad.Except (runExceptT, except)

serveWebSocket :: MonadSnap m => MVar AppST ->  m ()
serveWebSocket appST = runWebSocketsSnap (wsConduitApp appST)

wsConduitApp :: MVar AppST -> WS.ServerApp
wsConduitApp appST pending= do
  putStrLn "websocket connection accepted ..."
  conn <- WS.acceptRequest pending
  readMVar appST >>= WS.sendTextData conn . J.encode . WSInitResponse

  runConduit
    $ (forever $ liftIO (WS.receiveData conn) >>= yield . J.decode)
   .| CL.mapMaybe (id @(Maybe WSRequestMessage))
   .| C.mapM (wsHandle appST)
   .| C.mapM_ (WS.sendTextData conn . J.encode)

{--
wsHandle :: MVar AppST -> WSRequestMessage -> IO WSResponseMessage
wsHandle appST = \case
  HaskellCodeRunRequest r ->
    return . HaskellCodeRunResponse . mapLeft show =<<
      (I.runInterpreter . dynHaskell) r
  EventPulseAREQ name -> do
--    appST
    evalResult <- (I.runInterpreter . dynHaskell) "putStrLn \"hello world\""
    (return . EventPulseARES . mapLeft show) evalResult
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
--}

instance IsString I.InterpreterError where fromString = show
wsHandle :: MVar AppST -> WSRequestMessage -> IO WSResponseMessage    
wsHandle appST (HaskellCodeRunRequest r) =
  return . HaskellCodeRunResponse . mapLeft show =<<
    (I.runInterpreter . dynHaskell) r

wsHandle appST (EventPulseAREQ name) = do
  let getter = lens #dataNetwork . lens #eventPulses . at name
  eventPulseMaybe <- view getter <$> readMVar appST
--  let notFound = maybeToRight "EventPulse_Not_Found" eventPulses
--  evalResult' <- I.runInterpreter . dynHaskell . (cs . show) $ either
  evalResult <- runExceptT $ do
    eventPulse <- ExceptT $ return (maybeToRight "EventPulse_Not_Found" eventPulseMaybe)
    ExceptT $ (I.runInterpreter . dynHaskell . (cs . show)) eventPulse
--  (return . EventPulseARES . mapLeft show) evalResult
  undefined
  
wsHandle appST unknown = do
  putStrLn $ "CronTimerDeleteResponse: " ++ (show unknown)
  return . WSResponseUnknown $ unknown

dynHaskell :: T.Text -> I.Interpreter ()
dynHaskell stmt = do
  I.loadModules ["backend/src/Fn.hs" ]
  I.setTopLevelModules ["Fn"]
  I.set [I.languageExtensions I.:= [ I.OverloadedStrings
                                   , I.OverloadedLabels
                                   , I.TemplateHaskell
                                   , I.QuasiQuotes]]
  I.runStmt (cs stmt)
