import qualified Data.ByteString.Lazy as BSL
import System.Environment
import System.FilePath
import Codec.Binary.UTF8.String

import Network.SSH.Client.LibSSH2.Foreign
import Network.SSH.Client.LibSSH2

main = do
  args <- getArgs
  case args of
    ["command", user, host, port, cmd]  -> runCommand user host (read port) cmd
    ["send", user, host, port, path]    -> sendFile user host (read port) path
    ["receive", user, host, port, path] -> receiveFile user host (read port) path
    _ -> putStrLn "Synopsis: ssh-client ACTION USERNAME HOSTNAME PORT ARG"

runCommand login host port command =
  ssh login host port $ \s ->
    withChannel s $ \ch -> do
      channelExecute ch command
      result <- readAllChannel ch
      BSL.putStr result

sendFile login host port path =
  ssh login host port $ \s -> do
    sz <- scpSendFile s 0o644 path (takeFileName path)
    putStrLn $ "Sent: " ++ show sz ++ " bytes."

receiveFile login host port path =
  ssh login host port $ \s -> do
    sz <- scpReceiveFile s (takeFileName path) path
    putStrLn $ "Received: " ++ show sz ++ " bytes."

ssh login host port actions = do
  initialize True
  home <- getEnv "HOME"
  let known_hosts = home </> ".ssh" </> "known_hosts"
      public = home </> ".ssh" </> "id_rsa.pub"
      private = home </> ".ssh" </> "id_rsa"
  withSSH2 known_hosts public private "" login host port $ actions
  exit
