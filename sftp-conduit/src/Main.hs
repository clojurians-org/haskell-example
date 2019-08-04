{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude
import Control.Monad (when)
import Network.SSH.Client.LibSSH2
  ( Sftp, Session, sessionInit, sessionClose
  , checkHost
  , withSFTPUser, withOpenSftpFile)
import Network.SSH.Client.LibSSH2.Foreign
  ( SftpFileTransferFlags(..), KnownHostResult(..)
  , usernamePasswordAuth, sftpInit, sftpShutdown
  , sftpOpenFile, sftpCloseHandle, sftpWriteFileFromBS)
import System.Environment (getEnv)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Conduit (ConduitT, runConduitRes, bracketP, (.|))
import qualified Data.Conduit.Combinators as C

import Control.Monad.Trans (lift, liftIO)

main :: IO ()
main = do
  let 
    sftpSink = do
      let (hostname, port, username, password) = ("localhost", 22, "larluo", "larluo")
          filepath = "/Users/larluo/my-work/haskell-example/sftp-conduit/aaa.txt"
          flags = [FXF_WRITE, FXF_CREAT, FXF_TRUNC, FXF_EXCL]
      bracketP (sessionInit hostname port) sessionClose $ \s -> do
        liftIO $ usernamePasswordAuth s username password
        bracketP (sftpInit s) sftpShutdown $ \sftp -> do
          bracketP (sftpOpenFile sftp filepath 0o777 flags) sftpCloseHandle $ \sftph ->
            C.mapM (liftIO . sftpWriteFileFromBS sftph)
      
  runConduitRes $
    C.yieldMany ["aaa", "bbb", "ccc"]
    .| C.map (<> "\n")
    .| sftpSink
    .| C.sinkList
  putStrLn "finished!"
