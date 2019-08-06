{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

--module Backend.WebSocketServer (serveWebSocket) where
module Backend.WebSocketServer where

import Common.Class
import Common.Types
import Common.WebSocketMessage
import Common.ExampleData
import Prelude
import Text.Heredoc (str)

import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either.Combinators (mapLeft, maybeToLeft, maybeToRight)
import System.Random (randomRIO)

import GHC.Int (Int64)
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
import Control.Applicative ((<|>))
import Labels (lens)

import Control.Monad.Except (runExceptT, liftEither)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (runExceptT, ExceptT(..), except)

serveWebSocket :: MonadSnap m => MVar AppST ->  m ()
serveWebSocket appST = runWebSocketsSnap (wsConduitApp appST)

wsConduitApp :: MVar AppST -> WS.ServerApp
wsConduitApp appST pending= do
  putStrLn "websocket connection accepted ..."
  conn <- WS.acceptRequest pending

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

wsHandle :: MVar AppST -> WSRequestMessage -> IO WSResponseMessage
wsHandle appST AppInitREQ = do
  return . AppInitRES =<< readMVar appST
wsHandle appST (HaskellCodeRunRequest r) =
  return . HaskellCodeRunResponse . mapLeft show =<<
    (I.runInterpreter . dynHaskell) r

wsHandle appST (EventPulseAREQ name) = do
  faas <- readMVar appST  
  let getter = lens #dataNetwork . lens #eventPulses . at name

  let eventPulseMaybe = view getter faas
  evalResult <- runExceptT $ do
    eventPulse <- liftEither $ (maybeToRight "EventPulse_Not_Found" eventPulseMaybe)
    ExceptT $ mapLeft show <$> (I.runInterpreter . dynHaskell . toHaskellCode . toHaskellCodeBuilder faas) eventPulse
  (return . EventPulseARES . mapLeft show) evalResult
  
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


replRun :: IO ()
replRun = do
  let code = toHaskellCode . toHaskellCodeBuilder exampleFaasCenter $ (head exampleEventPulses)
  T.putStrLn code
  (mapLeft show <$> (I.runInterpreter . dynHaskell) code) >>= print
