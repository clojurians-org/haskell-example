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
import Backend.Streaming
import Prelude
import Text.Heredoc (str)

import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Either.Combinators (mapLeft, maybeToLeft, maybeToRight)
import System.Random (randomRIO)

import Data.Maybe (fromMaybe)
import Data.Functor ((<&>))

import Control.Exception (bracket, finally)
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
import Labels 

import Control.Monad.Except (runExceptT, liftEither)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (runExceptT, ExceptT(..), except)

import Data.Bits (Bits(..))
import Foreign.C.Types (CULong)

import Network.SSH.Client.LibSSH2
  ( Sftp, Session, sessionInit, sessionClose
  , checkHost
  , withSFTPUser, withOpenSftpFile, sftpListDir)
import Network.SSH.Client.LibSSH2.Foreign
  ( SftpFileTransferFlags(..), KnownHostResult(..)
  , SftpAttributes(..)
  , usernamePasswordAuth, sftpInit, sftpShutdown
  , sftpOpenFile, sftpCloseHandle, sftpWriteFileFromBS)

import qualified Database.Dpi as Oracle

serveWebSocket :: MonadSnap m => MVar AppST ->  m ()
serveWebSocket appST = runWebSocketsSnap (wsConduitApp appST)

wsConduitApp :: MVar AppST -> WS.ServerApp
wsConduitApp appST pending= do
  putStrLn "websocket connection accepted ..."
  conn <- WS.acceptRequest pending

  runConduit
    $ (forever $ liftIO (WS.receiveData conn) >>= yield . J.decode)
   .| CL.mapMaybe (id @(Maybe WSRequestMessage))
--   .| C.iterM print
   .| C.mapM (wsHandle appST)
--   .| C.iterM print         
   .| C.mapM_ (WS.sendTextData conn . J.encode)
   .| C.sinkNull

wsHandle :: MVar AppST -> WSRequestMessage -> IO WSResponseMessage
wsHandle appST AppInitREQ = do
  readMVar appST >>= putStrLn . ("INIT REQ" ++ ) . show
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
    let haskellCode = toHaskellCode $ toHaskellCodeBuilder faas eventPulse
    ExceptT $ mapLeft show <$> (I.runInterpreter . dynHaskell) haskellCode
  (return . EventPulseARES . mapLeft show) evalResult

wsHandle appST (DSOSQLCursorDatabaseRREQ cr "Oracle" database) = do
  DSOSQLCursorDatabaseRRES . Right <$> oracleShowTables cr database
  {--
  DSOSQLCursorDatabaseRRES . Right <$> return [ (#schema := "larluo", #table :="haskell")
                                              , (#schema := "larluo", #table := "clojure")]
  --}
wsHandle appST (DSOSQLCursorTableRREQ cr "Oracle" database (schema, table) ) = do
  DSOSQLCursorTableRRES . Right <$> oracleDescribeTable cr database (schema, table)

wsHandle appST (DSEFSSFtpDirectoryRREQ (Credential hostName hostPort username password) path) = do
  bracket (sessionInit (cs hostName) hostPort) sessionClose $ \s -> do
    liftIO $ usernamePasswordAuth s (cs username) (cs password)
    bracket (sftpInit s) sftpShutdown $ \sftp -> do
      sftpList <- sftpListDir sftp (cs $ fromMaybe "." path)
      return . DSEFSSFtpDirectoryRRES . Right $ sftpList <&> \(name, attrs) -> do
        let size = (fromIntegral . saFileSize) attrs
            ctime = (realToFrac . saMtime) attrs
            xtype = (parseSFtpEntryType . saPermissions) attrs
        SFtpEntry (cs name) xtype size ctime
  where
    parseSFtpEntryType ::  CULong -> SFtpEntryType
    parseSFtpEntryType = \case
      a | a .&. 0o0100000 /= 0 -> SFtpFille
        | a .&. 0o0040000 /= 0 -> SFtpDirectory
      _ -> SFtpUnknown        
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
  {--
  let code = toHaskellCode . toHaskellCodeBuilder exampleFaasCenter $ (head exampleEventPulses)
  T.putStrLn code
  (mapLeft show <$> (I.runInterpreter . dynHaskell) code) >>= print
  wsHandle undefined
    (DSEFSSFtpDirectoryRREQ (Credential "10.132.37.201" 22 "op" "op") (Just ".")) >>= print
  --}
  undefined
