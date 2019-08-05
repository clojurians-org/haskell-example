{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable, StandaloneDeriving #-}

#ifdef __APPLE__ 
#define _ANSI_SOURCE
#define __OSX_AVAILABLE_STARTING(_mac, _iphone)
#define __OSX_AVAILABLE_BUT_DEPRECATED(_macIntro, _macDep, _iphoneIntro, _iphoneDep) 
#endif

#include <libssh2.h>
#include <libssh2_sftp.h>

{# context lib="ssh2" prefix="libssh2" #}

module Network.SSH.Client.LibSSH2.Types
  (Session,
   KnownHosts,
   Channel,
   Sftp,
   SftpHandle,
   Agent,
   AgentPublicKey,
   ToPointer (..),
   Direction (..),
   int2dir,
   CStringCLen,
   Size, SSize,
   withCStringLenIntConv,
   peekCStringPtr,
   peekMaybeCStringPtr,
   channelFromPointer,
   knownHostsFromPointer,
   sessionFromPointer,
   sessionGetSocket,
   sessionSetSocket,
   channelSession,
   sftpFromPointer,
   sftpSession,
   sftpHandlePtr,
   sftpHandleFromPointer,
   sftpHandleSession,
   agentFromPointer,
   agentSession,
   agentPublicKeyFromPointer,
   withAgentPublicKey
  ) where

import Foreign
import Foreign.C.String
import Data.Generics
import Data.IORef
import Network.Socket

type Size = {# type size_t #}

type SSize = {# type ssize_t #}

type CStringCLen i = (CString, i)

withCStringLenIntConv :: (Integral i) => String -> (CStringCLen i -> IO a) -> IO a
withCStringLenIntConv str fn =
  withCStringLen str (\(ptr, len) -> fn (ptr, fromIntegral len))

peekCStringPtr :: Ptr CString -> IO String
peekCStringPtr ptr = peekCAString =<< peek ptr

peekMaybeCStringPtr :: Ptr CString -> IO (Maybe String)
peekMaybeCStringPtr ptr = do
  strPtr <- peek ptr
  if strPtr == nullPtr
    then return Nothing
    else Just `fmap` peekCAString strPtr


class ToPointer p where
  toPointer :: p -> Ptr ()

{# pointer *SESSION as CSession #}

data Session = Session { sessionPtr       :: CSession
                       , sessionSocketRef :: IORef (Maybe Socket)
                       }

sessionFromPointer :: Ptr () -> IO Session
sessionFromPointer ptr = do
  socketRef <- newIORef Nothing
  return $ Session (castPtr ptr) socketRef

sessionGetSocket :: Session -> IO (Maybe Socket)
sessionGetSocket = readIORef . sessionSocketRef

sessionSetSocket :: Session -> Maybe Socket -> IO ()
sessionSetSocket session = writeIORef (sessionSocketRef session)

deriving instance Eq Session
deriving instance Data Session
deriving instance Typeable Session

instance Show Session where
  show session = "<libssh2 session: " ++ show (sessionPtr session) ++ ">"

instance ToPointer Session where
  toPointer = castPtr . sessionPtr 

{# pointer *KNOWNHOSTS as KnownHosts newtype #}

knownHostsFromPointer :: Ptr () -> IO KnownHosts
knownHostsFromPointer ptr = return $ KnownHosts (castPtr ptr)

deriving instance Eq KnownHosts
deriving instance Data KnownHosts
deriving instance Typeable KnownHosts

instance Show KnownHosts where
  show (KnownHosts p) = "<libssh2 known hosts: " ++ show p ++ ">"

instance ToPointer KnownHosts where
  toPointer (KnownHosts p) = castPtr p

{# pointer *CHANNEL as CChannel #}

data Channel = Channel { channelPtr     :: CChannel
                       , channelSession :: Session
                       }

channelFromPointer :: Session -> Ptr () -> IO Channel
channelFromPointer session ptr = return $ Channel (castPtr ptr) session

deriving instance Eq Channel
deriving instance Data Channel
deriving instance Typeable Channel

instance Show Channel where
  show channel = "<libssh2 channel: " ++ show (channelPtr channel) ++ ">"

instance ToPointer Channel where
  toPointer = castPtr . channelPtr 

-- | Session directions
data Direction = INBOUND | OUTBOUND
  deriving (Eq, Show)

int2dir :: (Eq a, Num a, Show a) => a -> [Direction]
int2dir 1 = [INBOUND]
int2dir 2 = [OUTBOUND]
int2dir 3 = [INBOUND, OUTBOUND]
int2dir x = error $ "Unknown direction: " ++ show x

--
-- | Sftp support
--

sftpFromPointer :: Session -> Ptr () -> IO Sftp
sftpFromPointer session ptr = return $ Sftp (castPtr ptr) session

{# pointer *SFTP as CSftp #}

data Sftp = Sftp { sftpPtr :: CSftp
                 , sftpSession :: Session
                 }

instance Show Sftp where
  show sftp = "<libssh2 sftp: " ++ show (sftpPtr sftp) ++ ">"

instance ToPointer Sftp where
  toPointer = castPtr . sftpPtr

sftpHandleFromPointer :: Sftp -> Ptr () -> IO SftpHandle
sftpHandleFromPointer sftp ptr = return $ SftpHandle (castPtr ptr) sftp

{# pointer *SFTP_HANDLE as CSftpHandle #}

data SftpHandle = SftpHandle { sftpHandlePtr :: CSftpHandle
                             , sftpHandleSession :: Sftp
                             }

instance Show SftpHandle where
  show handle = "<libssh2 sftp handle: " ++ show (sftpHandlePtr handle) ++ ">"

instance ToPointer SftpHandle where
  toPointer = castPtr . sftpHandlePtr


--
-- | Agent support
--

agentFromPointer :: Session -> Ptr () -> IO Agent
agentFromPointer session ptr = return $ Agent (castPtr ptr) session

{# pointer *AGENT as CAgent #}

data Agent = Agent { agentPtr :: CAgent, agentSession :: Session }

instance Show Agent where
  show agent = "<libssh2 agent: " ++ show (agentPtr agent) ++ ">"

instance ToPointer Agent where
  toPointer = castPtr . agentPtr

{# pointer *agent_publickey as AgentPublicKey foreign newtype #}

agentPublicKeyFromPointer :: Ptr () -> IO AgentPublicKey
agentPublicKeyFromPointer ptr = do
  newPtr <- newForeignPtr_ ptr
  return $ AgentPublicKey $ castForeignPtr newPtr

deriving instance Eq AgentPublicKey
deriving instance Data AgentPublicKey
deriving instance Typeable AgentPublicKey

instance Show AgentPublicKey where
  show (AgentPublicKey p) = "<libssh2 agent publickey: " ++ show p ++ ">"
