{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable, FlexibleInstances #-}

#ifdef __APPLE__
#define _ANSI_SOURCE
#define __OSX_AVAILABLE_STARTING(_mac, _iphone)
#define __OSX_AVAILABLE_BUT_DEPRECATED(_macIntro, _macDep, _iphoneIntro, _iphoneDep)
#endif

#include <libssh2.h>
#include <libssh2_sftp.h>

{# context lib="ssh2" prefix="libssh2" #}

module Network.SSH.Client.LibSSH2.Errors
  (-- * Types
   ErrorCode (..),
   SftpErrorCode (..),
   NULL_POINTER,

   -- * Utilities
   IntResult (..),

   -- * Functions
   getLastError,
   getLastSftpError,
   handleInt,
   handleBool,
   handleNullPtr,
   int2error, error2int,
   int2sftperror, sftperror2int,
   blockedDirections,
   threadWaitSession
  ) where

import Control.Exception
import Data.Generics
import Foreign
import Foreign.C.Types

import Network.SSH.Client.LibSSH2.Types
import Network.SSH.Client.LibSSH2.WaitSocket

-- | Error codes returned by libssh2.
data ErrorCode =
    NONE
  | SOCKET_NONE
  | BANNER_RECV
  | BANNER_SEND
  | INVALID_MAC
  | KEX_FALIURE
  | ALLOC
  | SOCKET_SEND
  | KEY_EXCHANGE_FAILURE
  | TIMEOUT
  | HOSTKEY_INIT
  | HOSTKEY_SIGN
  | DECRYPT
  | SOCKET_DISCONNECT
  | PROTO
  | PASSWORD_EXPIRED
  | FILE
  | METHOD_NONE
  | AUTHENTICATION_FAILED
  | PUBLICKEY_UNVERIFIED
  | CHANNEL_OUTOFORDER
  | CHANNEL_FAILURE
  | CHANNEL_REQUEST_DENIED
  | CHANNEL_UNKNOWN
  | CHANNEL_WINDOW_EXCEEDED
  | CHANNEL_PACKET_EXCEEDED
  | CHANNEL_CLOSED
  | CHANNEL_EOF_SENT
  | SCP_PROTOCOL
  | ZLIB
  | SOCKET_TIMEOUT
  | SFTP_PROTOCOL
  | REQUEST_DENIED
  | METHOD_NOT_SUPPORTED
  | INVAL
  | INVALID_POLL_TYPE
  | PUBLICKEY_PROTOCOL
  | EAGAIN
  | BUFFER_TOO_SMALL
  | BAD_USE
  | COMPRESS
  | OUT_OF_BOUNDARY
  | AGENT_PROTOCOL
  | SOCKET_RECV
  | ENCRYPT
  | BAD_SOCKET
  | ERROR_KNOWN_HOSTS
  deriving (Eq, Show, Ord, Enum, Data, Typeable)

instance Exception ErrorCode

error2int :: (Num i) => ErrorCode -> i
error2int = fromIntegral . negate . fromEnum

int2error :: (Integral i) => i -> ErrorCode
int2error = toEnum . negate . fromIntegral

-- | Exception to throw when null pointer received
-- from libssh2.
data NULL_POINTER = NULL_POINTER
  deriving (Eq, Show, Data, Typeable)

instance Exception NULL_POINTER

class IntResult a where
  intResult :: a -> Int

instance IntResult Int where
  intResult = id

instance IntResult (Int, a) where
  intResult = fst

instance IntResult (Int, a, b) where
  intResult = \(i, _, _) -> i

instance IntResult (Int, a, b, c) where
  intResult = \(i, _, _, _) -> i

instance IntResult CInt where
  intResult = fromIntegral

instance IntResult CLong where
  intResult = fromIntegral

instance IntResult CLLong where
  intResult = fromIntegral

{# fun session_last_error as getLastError_
  { toPointer `Session',
    alloca- `String' peekCStringPtr*,
    castPtr `Ptr Int',
    `Int' } -> `Int' #}

-- | Get last error information.
getLastError :: Session -> IO (Int, String)
getLastError s = getLastError_ s nullPtr 0

-- | Throw an exception if negative value passed,
-- or return unchanged value.
handleInt :: (IntResult a, SshCtx ctx) => Maybe ctx -> IO a -> IO a
handleInt s io = do
  x <- io
  let r = intResult x
  if r < 0
    then case int2error r of
           EAGAIN -> threadWaitSession s >> handleInt s io
           err    ->
             case s of
               Nothing  -> throw err
               Just ctx -> throwCtxSpecificError ctx err
    else return x

handleBool :: CInt -> IO Bool
handleBool x
  | x == 0 = return False
  | x > 0  = return True
  | otherwise = throw (int2error x)

-- | Throw an exception if null pointer passed,
-- or return it casted to right type.
handleNullPtr :: (SshCtx c) => Maybe c -> (Ptr () -> IO a) -> IO (Ptr ()) -> IO a
handleNullPtr m_ctx fromPointer io = do
  ptr <- io
  if ptr == nullPtr
    then case m_ctx of
      Nothing  -> throw NULL_POINTER
      Just ctx -> do
        let session = getSession ctx
        (r, _) <- getLastError session
        case int2error r of
          EAGAIN -> threadWaitSession (Just session) >> handleNullPtr m_ctx fromPointer io
          err    -> throwCtxSpecificError ctx err
    else fromPointer ptr

-- | Get currently blocked directions
{# fun session_block_directions as blockedDirections
  { toPointer `Session' } -> `[Direction]' int2dir #}

threadWaitSession :: (SshCtx ctx) => Maybe ctx -> IO ()
threadWaitSession Nothing = error "EAGAIN thrown without session present"
threadWaitSession (Just ctx) = do
  let s = getSession ctx
  mSocket <- sessionGetSocket s
  case mSocket of
    Nothing -> error "EAGAIN thrown on session without socket"
    Just socket -> do
      dirs <- blockedDirections s
      if (OUTBOUND `elem` dirs)
        then threadWaitWrite socket
        else threadWaitRead socket

-- | Sftp

{# fun sftp_last_error as getLastSftpError_
  {toPointer `Sftp'} -> `Int' #}

-- | Get last sftp related error.
getLastSftpError :: Sftp -> IO Int
getLastSftpError sftp = getLastSftpError_ sftp

sftperror2int :: (Num i) => SftpErrorCode -> i
sftperror2int = fromIntegral . fromEnum

int2sftperror :: (Integral i) => i -> SftpErrorCode
int2sftperror = toEnum . fromIntegral

-- | Sftp error code returning from libssh2
data SftpErrorCode =
    FX_OK
  | FX_EOF
  | FX_NO_SUCH_FILE
  | FX_PERMISSION_DENIED
  | FX_FAILURE
  | FX_BAD_MESSAGE
  | FX_NO_CONNECTION
  | FX_CONNECTION_LOST
  | FX_OP_UNSUPPORTED
  | FX_INVALID_HANDLE
  | FX_NO_SUCH_PATH
  | FX_FILE_ALREADY_EXISTS
  | FX_WRITE_PROTECT
  | FX_NO_MEDIA
  | FX_NO_SPACE_ON_FILESYSTEM
  | FX_QUOTA_EXCEEDED
  | FX_UNKNOWN_PRINCIPAL
  | FX_LOCK_CONFLICT
  | FX_DIR_NOT_EMPTY
  | FX_NOT_A_DIRECTORY
  | FX_INVALID_FILENAME
  | FX_LINK_LOOP
  deriving (Eq, Show, Ord, Enum, Data, Typeable)

instance Exception SftpErrorCode


class SshCtx a where
  getSession :: a -> Session
  throwCtxSpecificError :: a -> ErrorCode -> IO b

instance SshCtx Session where
  getSession = id
  throwCtxSpecificError _ er = throw er

instance SshCtx Sftp where
  getSession = sftpSession

  throwCtxSpecificError ctx SFTP_PROTOCOL = do
    er <- getLastSftpError ctx
    throw (int2sftperror er)
  throwCtxSpecificError _ er = throw er

instance SshCtx SftpHandle where
  getSession = getSession . sftpHandleSession

  throwCtxSpecificError ctx =
    throwCtxSpecificError (sftpHandleSession ctx)

instance SshCtx Agent where
  getSession = getSession . agentSession
  throwCtxSpecificError _ er = throw er