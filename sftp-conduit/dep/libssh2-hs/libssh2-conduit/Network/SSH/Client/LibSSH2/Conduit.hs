{-# LANGUAGE FlexibleContexts #-}
module Network.SSH.Client.LibSSH2.Conduit
  (sourceChannel,
   sinkChannel,
   CommandsHandle,
   execCommand,
   getReturnCode
  ) where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.STM
import Data.Conduit
import qualified Data.ByteString as B

import Network.SSH.Client.LibSSH2.Foreign
import Network.SSH.Client.LibSSH2

-- | Stream data from @Channel@.
sourceChannel :: MonadIO m => Channel -> Source m B.ByteString
sourceChannel ch = src
  where
    src = do
      res <- liftIO $ readChannel ch 0x400
      if B.length res > 0
        then do
             yield res
             src
        else return ()

-- | Stream data to @Channel@.
sinkChannel :: MonadIO m => Channel -> Sink B.ByteString m ()
sinkChannel channel =
    loop
  where
    loop = await >>= maybe (return ()) (\bs -> lift (liftIO $ writeChannel channel bs) >> loop)

-- | Execute one command and read it's output lazily.
-- If first argument is True, then you *must* get return code
-- using getReturnCode on returned CommandsHandle. Moreover,
-- you *must* guarantee that getReturnCode will be called
-- only when all command output will be read.
execCommand :: MonadIO m
            => Bool                          -- ^ Set to True if you want to get return code when command will terminate.
            -> Session
            -> String                        -- ^ Command
            -> IO (Maybe CommandsHandle, Source m B.ByteString) 
execCommand b s cmd = do
  (ch, channel) <- initCH b s
  let src = execCommandSrc ch channel cmd
  return (if b then Just ch else Nothing, src)

-- | Handles channel opening and closing.
data CommandsHandle = CommandsHandle {
  chReturnCode :: Maybe (TMVar Int),
  chChannel :: TMVar Channel,
  chChannelClosed :: TVar Bool }

initCH :: Bool -> Session -> IO (CommandsHandle, Channel)
initCH False s = do
  c <- newTVarIO False
  ch <- newEmptyTMVarIO
  channel <- openCH ch s
  return (CommandsHandle Nothing ch c, channel)
initCH True s = do
  r <- newEmptyTMVarIO
  c <- newTVarIO False
  ch <- newEmptyTMVarIO
  channel <- openCH ch s
  return (CommandsHandle (Just r) ch c, channel)

openCH :: TMVar Channel -> Session -> IO Channel
openCH var s = do
      ch <- openChannelSession s
      atomically $ putTMVar var ch
      return ch

-- | Get return code for previously run command.
-- It will fail if command was run using execCommand False.
-- Should be called only when all command output is read.
getReturnCode :: CommandsHandle -> IO Int
getReturnCode ch = do
  c <- atomically $ readTVar (chChannelClosed ch)
  if c
    then do
      case chReturnCode ch of
        Nothing -> fail "Channel already closed and no exit code return was set up for command."
        Just v -> atomically $ takeTMVar v
    else do
      channel <- atomically $ takeTMVar (chChannel ch)
      cleanupChannel ch channel
      atomically $ writeTVar (chChannelClosed ch) True
      case chReturnCode ch of
        Nothing -> fail "No exit code return was set up for commnand."
        Just v  -> do
                   rc <- atomically $ takeTMVar v
                   return rc
    
execCommandSrc :: MonadIO m => CommandsHandle -> Channel -> String -> Source m B.ByteString
execCommandSrc var channel command = src
  where
    src = do
      liftIO $ channelExecute channel command
      pullAnswer channel
    
    pullAnswer ch = do
      res <- liftIO $ readChannel ch 0x400
      if B.length res > 0
        then do
             yield res
             pullAnswer ch
        else do
             liftIO $ cleanupChannel var ch
             return ()

-- | Close Channel and write return code
cleanupChannel :: CommandsHandle -> Channel -> IO ()
cleanupChannel ch channel = do
  c <- atomically $ readTVar (chChannelClosed ch)
  when (not c) $ do
    closeChannel channel
    case chReturnCode ch of
      Nothing -> return ()
      Just v  -> do
                 exitStatus <- channelExitStatus channel
                 atomically $ putTMVar v exitStatus
    closeChannel channel
    freeChannel channel
    atomically $ writeTVar (chChannelClosed ch) True
    return ()

