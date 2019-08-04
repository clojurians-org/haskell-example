{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conduit
import Control.Concurrent
import Control.Concurrent.Async (concurrently)
import Control.Monad
import Data.Conduit.Network
import System.Environment
import System.FilePath

import Network.SSH.Client.LibSSH2.Foreign
import Network.SSH.Client.LibSSH2.Conduit
import Network.SSH.Client.LibSSH2

import Network.Socket hiding (send, sendTo, recv, recvFrom)

import qualified Control.Exception as E

main :: IO ()
main = do
  args <- getArgs
  case args of
    [user, host, port, srcport, dstport] -> run user host (read port) (read srcport) (read dstport)
    _ -> putStrLn "Synopsis: hs-ssh-forwarder USERNAME HOSTNAME SSHPORT SRCPORT DSTPORT"

run :: String -> String -> PortNumber -> PortNumber -> PortNumber -> IO ()
run username host port srcport dstport = do
  initialize True

  E.bracket (open srcport) (\s -> close s >> exit) $ \sock -> void . forever $ do
    (conn, _) <- accept sock
    forkFinally
      (handleConn username host port dstport conn)
      (const $ close conn)

open :: PortNumber -> IO Socket
open srcport = do
  sock <- socket AF_INET6 Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  setSocketOption sock ReusePort 1
  bind sock $ SockAddrInet6 srcport 0 iN6ADDR_ANY 0
  listen sock 5
  return sock

handleConn :: String -> String -> PortNumber -> PortNumber -> Socket -> IO ()
handleConn login host sshport dstport conn = ssh login host (fromIntegral sshport) $ \session -> do
  channel <- directTcpIpEx session "localhost" (fromIntegral dstport) host (fromIntegral sshport)

  void $ concurrently
      (runConduit $ sourceChannel channel .| sinkSocket conn)
      (runConduit $ sourceSocket conn .| sinkChannel channel)

  closeChannel channel
  freeChannel channel

ssh login host port actions = do
  home <- getEnv "HOME"
  let known_hosts = home </> ".ssh" </> "known_hosts"
      public = home </> ".ssh" </> "id_rsa.pub"
      private = home </> ".ssh" </> "id_rsa"
  withSSH2 known_hosts public private "" login host port $ actions
