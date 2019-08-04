{-# LANGUAGE ScopedTypeVariables #-}
module Network.SSH.Client.LibSSH2
  (-- * Types
   Session, Channel, KnownHosts, Sftp, SftpHandle,
   SftpAttributes, SftpList, SftpFileTransferFlags,

   -- * Functions
   withSSH2,
   withSSH2User,
   withSSH2Agent,
   withSession,
   withChannel,
   withChannelBy,
   checkHost,
   readAllChannel,
   writeAllChannel,
   scpSendFile,
   scpReceiveFile,
   runShellCommands,
   execCommands,
   directTcpIpEx,

   -- * Sftp Functions
   withSFTP,
   withSFTPUser,
   withSftpSession,
   withOpenSftpFile,
   sftpListDir,
   sftpRenameFile,
   sftpSendFile, sftpSendFromHandle,
   sftpReceiveFile, sftpReadFileToHandler,
   sftpFstat,
   sftpDeleteFile,

   -- * Utilities
   socketConnect,
   sessionInit,
   sessionClose,
  ) where

import Control.Monad
import Control.Exception as E
import Network.Socket
import System.IO
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Char8 as BSSC
import qualified Data.ByteString.Lazy as BSL

import Network.SSH.Client.LibSSH2.Types
import Network.SSH.Client.LibSSH2.Foreign

-- | Similar to Network.connectTo, but does not socketToHandle.
socketConnect :: String -> Int -> IO Socket
socketConnect hostname port = do
  let hints = defaultHints { addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints) (Just hostname) (Just $ show port)
  bracketOnError
    (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
    (close)
    (\sock -> do
       connect sock $ addrAddress addr
       return sock)

-- | Execute some actions within SSH2 connection.
-- Uses public key authentication.
withSSH2 :: FilePath          -- ^ Path to known_hosts file
         -> FilePath          -- ^ Path to public key file
         -> FilePath          -- ^ Path to private key file
         -> String            -- ^ Passphrase
         -> String            -- ^ Remote user name
         -> String            -- ^ Remote host name
         -> Int               -- ^ Remote port number (usually 22)
         -> (Session -> IO a) -- ^ Actions to perform on session
         -> IO a
withSSH2 known_hosts public private passphrase login hostname port fn =
  withSession hostname port $ \s -> do
    r <- checkHost s hostname port known_hosts
    when (r == MISMATCH) $
      error $ "Host key mismatch for host " ++ hostname
    publicKeyAuthFile s login public private passphrase
    fn s

-- | Execute some actions within SSH2 connection.
-- Uses agent based public key authentication.
withSSH2Agent :: String            -- ^ Path to known_hosts file
              -> String            -- ^ Remote user name
              -> String            -- ^ Remote host name
              -> Int               -- ^ Remote port number (usually 22)
              -> (Session -> IO a) -- ^ Actions to perform on session
              -> IO a
withSSH2Agent known_hosts login hostname port fn =
  withSession hostname port $ \s -> do
    r <- checkHost s hostname port known_hosts
    when (r == MISMATCH) $
      error $ "host key mismatch for host " ++ hostname
    E.bracket (agentInit s) agentFree $ \a ->
      E.bracket_ (agentConnect a) (agentDisconnect a) (act s login a fn)
    where
      act s u a f = do
          agentListIdentities a
          agentAuthenticate u a
          f s

-- | Execute some actions within SSH2 connection.
-- Uses username/password authentication.
withSSH2User :: FilePath          -- ^ Path to known_hosts file
         -> String            -- ^ Remote user name
         -> String            -- ^ Remote password
         -> String            -- ^ Remote host name
         -> Int               -- ^ Remote port number (usually 22)
         -> (Session -> IO a) -- ^ Actions to perform on session
         -> IO a
withSSH2User known_hosts login password hostname port fn =
  withSession hostname port $ \s -> do
    r <- checkHost s hostname port known_hosts
    when (r == MISMATCH) $
      error $ "Host key mismatch for host " ++ hostname
    usernamePasswordAuth s login password
    fn s

-- | Execute some actions within SSH2 session
withSession :: String            -- ^ Remote host name
            -> Int               -- ^ Remote port number (usually 22)
            -> (Session -> IO a) -- ^ Actions to perform on handle and session
            -> IO a
withSession hostname port = E.bracket (sessionInit hostname port) sessionClose

--  | Initialize session to the gived host
sessionInit :: String -> Int -> IO Session
sessionInit hostname port = do
      sock <- socketConnect hostname port
      session <- initSession
      setBlocking session False
      handshake session sock
      return session

--  | Close active session
sessionClose :: Session -> IO ()
sessionClose session = do
      disconnectSession session "Done."
      sessionGetSocket session >>= maybe (pure ()) close
      freeSession session



--  | Check remote host against known hosts list
checkHost :: Session
          -> String             -- ^ Remote host name
          -> Int                -- ^ Remote port number (usually 22)
          -> FilePath           -- ^ Path to known_hosts file
          -> IO KnownHostResult
checkHost s host port path = do
  kh <- initKnownHosts s
  _numKnownHosts <- knownHostsReadFile kh path
  (hostkey, _keylen, _keytype) <- getHostKey s
  result <- checkKnownHost kh host port hostkey [TYPE_PLAIN, KEYENC_RAW]
  freeKnownHosts kh
  return result

-- | Execute some actions withing SSH2 channel
withChannel :: Session -> (Channel -> IO a) -> IO (Int, a)
withChannel s = withChannelBy (openChannelSession s) id

-- | Read all data from the channel
--
-- Although this function returns a lazy bytestring, the data is /not/ read
-- lazily.
readAllChannel :: Channel -> IO BSL.ByteString
readAllChannel ch = go []
  where
    go :: [BSS.ByteString] -> IO BSL.ByteString
    go acc = do
      bs <- readChannel ch 0x400
      if BSS.length bs > 0
        then go (bs : acc)
        else return (BSL.fromChunks $ reverse acc)

readAllChannelNonBlocking :: Channel -> IO BSL.ByteString
readAllChannelNonBlocking ch = go []
  where
    go :: [BSS.ByteString] -> IO BSL.ByteString
    go acc = do
      bs <- do readable <- pollChannelRead ch
               if readable
                 then readChannel ch 0x400
                 else return BSS.empty
      if BSS.length bs > 0
        then go (bs : acc)
        else return (BSL.fromChunks $ reverse acc)

-- | Write a lazy bytestring to the channel
writeAllChannel :: Channel -> BSL.ByteString -> IO ()
writeAllChannel ch = mapM_ (writeChannel ch) . BSL.toChunks

runShellCommands :: Session -> [String] -> IO (Int, [BSL.ByteString])
runShellCommands s commands = withChannel s $ \ch -> do
  requestPTY ch "linux"
  channelShell ch
  _hello <- readAllChannelNonBlocking ch
  out <- forM commands $ \cmd -> do
             writeChannel ch (BSSC.pack $ cmd ++ "\n")
             r <- readAllChannelNonBlocking ch
             return r
  channelSendEOF ch
  return out

execCommands :: Session -> [String] -> IO (Int, [BSL.ByteString])
execCommands s commands = withChannel s $ \ch ->
  forM commands $ \cmd -> do
      channelExecute ch cmd
      readAllChannel ch

-- | Send a file to remote host via SCP.
-- Returns size of sent data.
scpSendFile :: Session
            -> Int       -- ^ File creation mode (0o777, for example)
            -> FilePath  -- ^ Path to local file
            -> FilePath  -- ^ Remote file path
            -> IO Integer
scpSendFile s mode local remote = do
  h <- openFile local ReadMode
  size <- hFileSize h
  (_, result) <- withChannelBy (scpSendChannel s remote mode (fromIntegral size) 0 0) id $ \ch -> do
    written <- writeChannelFromHandle ch h
    channelSendEOF ch
    channelWaitEOF ch
    return written
  hClose h
  return result

-- | Receive file from remote host via SCP.
-- Returns size of received data.
scpReceiveFile :: Session   --
               -> FilePath  -- ^ Remote file path
               -> FilePath  -- ^ Path to local file
               -> IO Integer
scpReceiveFile s remote local = do
  h <- openFile local WriteMode
  (_, result) <- withChannelBy (scpReceiveChannel s remote) fst $ \(ch, fileSize) -> do
    readChannelToHandle ch h fileSize
  hClose h
  return result

-- | Generalization of 'withChannel'
withChannelBy :: IO a            -- ^ Create a channel (and possibly other stuff)
              -> (a -> Channel)  -- ^ Extract the channel from "other stuff"
              -> (a -> IO b)     -- ^ Actions to execute on the channel
              -> IO (Int, b)     -- ^ Channel exit status and return value
withChannelBy createChannel extractChannel actions = do
  stuff <- createChannel
  let ch = extractChannel stuff
  result <- actions stuff
  closeChannel ch
  exitStatus <- channelExitStatus ch
  freeChannel ch
  return (exitStatus, result)

-- | Execute some actions within SFTP connection.
-- Uses public key authentication.
withSFTP :: FilePath          -- ^ Path to known_hosts file
         -> FilePath          -- ^ Path to public key file
         -> FilePath          -- ^ Path to private key file
         -> String            -- ^ Passphrase
         -> String            -- ^ Remote user name
         -> String            -- ^ Remote host name
         -> Int               -- ^ Remote port number (usually 22)
         -> (Sftp -> IO a)    -- ^ Actions to perform on sftp session
         -> IO a
withSFTP known_hosts public private passphrase login hostname port fn =
  withSession hostname port $ \s -> do
    r <- checkHost s hostname port known_hosts
    when (r == MISMATCH) $
      error $ "Host key mismatch for host " ++ hostname
    publicKeyAuthFile s login public private passphrase
    withSftpSession s fn

-- | Execute some actions within SFTP connection.
-- Uses username/password authentication.
withSFTPUser :: FilePath          -- ^ Path to known_hosts file
             -> String            -- ^ Remote user name
             -> String            -- ^ Remote password
             -> String            -- ^ Remote host name
             -> Int               -- ^ Remote port number (usually 22)
             -> (Sftp -> IO a)    -- ^ Actions to perform on sftp session
             -> IO a
withSFTPUser known_hosts login password hostname port fn =
  withSession hostname port $ \s -> do
    r <- checkHost s hostname port known_hosts
    when (r == MISMATCH) $
      error $ "Host key mismatch for host " ++ hostname
    usernamePasswordAuth s login password
    withSftpSession s fn

-- | Execute some actions within SFTP session
withSftpSession :: Session           -- ^ Remote host name
                -> (Sftp -> IO a)    -- ^ Actions to perform on sftp session
                -> IO a
withSftpSession session =
  E.bracket (sftpInit session) sftpShutdown

type SftpList = [(BSS.ByteString, SftpAttributes)]

-- | Reads directory information
-- Returns the list of files with attributes, directory . and ..
-- are not excluded
sftpListDir :: Sftp        -- ^ Opened sftp session
            -> FilePath    -- ^ Remote directory to read
            -> IO SftpList
sftpListDir sftp path =
  let
    collectFiles :: SftpHandle -> SftpList -> IO SftpList
    collectFiles h acc = do
      v <- sftpReadDir h
      case v of
        Nothing -> return acc
        Just r  -> collectFiles h (r : acc)
  in
    withDirList sftp path $ \h ->
      collectFiles h []

withDirList :: Sftp
            -> FilePath
            -> (SftpHandle -> IO a)
            -> IO a
withDirList sftp path = E.bracket (sftpOpenDir sftp path) sftpCloseHandle


-- | Send a file to remote host via SFTP
-- Returns size of sent data.
sftpSendFile :: Sftp      -- ^ Opened sftp session
             -> FilePath  -- ^ Path to local file
             -> FilePath  -- ^ Remote file path
             -> Int       -- ^ File creation mode (0o777, for example)
             -> IO Integer
sftpSendFile sftp local remote mode =
  withFile local ReadMode $ \fh ->
    sftpSendFromHandle sftp fh remote mode

-- | Send a file to remote host via SFTP
-- Returns size of sent data.
sftpSendFromHandle :: Sftp      -- ^ Opened sftp session
                   -> Handle    -- ^ Handle to read from
                   -> FilePath  -- ^ Remote file path
                   -> Int       -- ^ File creation mode (0o777, for example)
                   -> IO Integer
sftpSendFromHandle sftp fh remote mode = do
  let flags = [FXF_WRITE, FXF_CREAT, FXF_TRUNC, FXF_EXCL]
  withOpenSftpFile sftp remote mode flags $ \sftph ->
    sftpWriteFileFromHandler sftph fh

-- | Received a file from remote host via SFTP
-- Returns size of received data.
sftpReceiveFile :: Sftp      -- ^ Opened sftp session
                -> FilePath  -- ^ Path to local file
                -> FilePath  -- ^ Remote file path
                -> IO Integer
sftpReceiveFile sftp local remote =
  withFile local WriteMode $ \fh ->
    sftpReceiveToHandle sftp remote fh

-- | Received a file from remote host via SFTP
-- Returns size of received data.
sftpReceiveToHandle :: Sftp      -- ^ Opened sftp session
                    -> FilePath  -- ^ Path to remote file
                    -> Handle    -- ^ Open handle to write to
                    -> IO Integer
sftpReceiveToHandle sftp remote fh = do
  result <- withOpenSftpFile sftp remote 0 [FXF_READ] $ \sftph -> do
    fstat <- sftpFstat sftph
    sftpReadFileToHandler sftph fh (fromIntegral $ saFileSize fstat)
  return $ fromIntegral result

withOpenSftpFile :: Sftp
                 -> FilePath
                 -> Int
                 -> [SftpFileTransferFlags]
                 -> (SftpHandle -> IO a)
                 -> IO a
withOpenSftpFile sftp path mode flags =
  E.bracket (sftpOpenFile sftp path mode flags) sftpCloseHandle
