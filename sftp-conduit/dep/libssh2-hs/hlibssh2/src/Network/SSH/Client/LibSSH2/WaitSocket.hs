{-# LANGUAGE CPP, ForeignFunctionInterface #-}
-- | Block until a read or write operation on a socket would succeed
--
-- On most platforms this uses 'Control.Concurrent.threadWaitRead' or
-- 'Conctrol.Concurrent.threadWaitWrite', but on Windows we need to do
-- something different. 
--
-- See <http://hackage.haskell.org/trac/ghc/ticket/5797>.
module Network.SSH.Client.LibSSH2.WaitSocket 
  ( threadWaitRead
  , threadWaitWrite
  ) where

import Network.Socket(Socket,fdSocket)
import System.Posix.Types(Fd(Fd))

#ifdef mingw32_HOST_OS
import Control.Concurrent(forkIO,newEmptyMVar,putMVar,takeMVar)
import Control.Exception(IOException,throwIO,try)
import Control.Exception.Base(mask_)
import Foreign.C.Error(throwErrnoIfMinus1_)
import Foreign.C.Types(CInt(CInt))
import System.IO(hWaitForInput,stdin)
#else
import qualified GHC.Conc (threadWaitRead, threadWaitWrite)
#endif

threadWaitRead :: Socket -> IO ()
threadWaitRead = threadWaitRead_ . Fd . fdSocket

threadWaitWrite :: Socket -> IO ()
threadWaitWrite = threadWaitWrite_ . Fd . fdSocket

-- | Block the current thread until data is available to read on the
-- given file descriptor (GHC only).
--
-- This will throw an 'IOError' if the file descriptor was closed
-- while this thread was blocked.  To safely close a file descriptor
-- that has been used with 'threadWaitRead', use
-- 'GHC.Conc.closeFdWith'.
threadWaitRead_ :: Fd -> IO ()
threadWaitRead_ fd
#ifdef mingw32_HOST_OS
  -- We have no IO manager implementing threadWaitRead on Windows.
  -- fdReady does the right thing, but we have to call it in a
  -- separate thread, otherwise threadWaitRead won't be interruptible,
  -- and this only works with -threaded.
  | threaded  = withThread (waitFd fd 0)
  | otherwise = case fd of
      0 -> do 
        -- hWaitForInput does work properly, but we can only
        -- do this for stdin since we know its FD.
        _ <- hWaitForInput stdin (-1)
        return ()
      _ -> 
        error "threadWaitRead requires -threaded on Windows, or use System.IO.hWaitForInput"
#else
  = GHC.Conc.threadWaitRead fd
#endif

-- | Block the current thread until data can be written to the
-- given file descriptor (GHC only).
-- This will throw an 'IOError' if the file descriptor was closed
-- while this thread was blocked.  To safely close a file descriptor
-- that has been used with 'threadWaitWrite', use
-- 'GHC.Conc.closeFdWith'.
threadWaitWrite_ :: Fd -> IO ()
threadWaitWrite_ fd
#ifdef mingw32_HOST_OS
  | threaded  = withThread (waitFd fd 1)
  | otherwise = error "threadWaitWrite requires -threaded on Windows"
#else
  = GHC.Conc.threadWaitWrite fd
#endif

#ifdef mingw32_HOST_OS
foreign import ccall unsafe "rtsSupportsBoundThreads" threaded:: Bool

withThread :: IO a -> IO a
withThread io = do
  m <- newEmptyMVar
  _ <- mask_ $ forkIO $ try io >>= putMVar m
  x <- takeMVar m
  case x of
    Right a -> return a
    Left e  -> throwIO (e :: IOException)

-- The last argument can be 1 (true) because this will only be applied to
-- sockets 
waitFd :: Fd -> CInt -> IO ()
waitFd fd write = 
    throwErrnoIfMinus1_ "fdReady" $ fdReady (fromIntegral fd) write iNFINITE 1 
  where
    iNFINITE :: CInt
    iNFINITE = 0xFFFFFFFF -- urgh

foreign import ccall safe "fdReady"
  fdReady:: CInt -- ^ fd
         -> CInt -- ^ write
         -> CInt -- ^ msecs
         -> CInt -- ^ isSock
         -> IO CInt
#endif
