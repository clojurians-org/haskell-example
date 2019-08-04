{-# LANGUAGE BangPatterns #-}

import Data.Conduit
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as CL
import System.Environment
import System.FilePath
import qualified Data.Text.IO as TIO
import System.IO

import Network.SSH.Client.LibSSH2.Foreign
import Network.SSH.Client.LibSSH2.Conduit
import Network.SSH.Client.LibSSH2

main = do
  args <- getArgs
  case args of
    [user, host, port, cmd]  -> ssh user host (read port) cmd
    _ -> putStrLn "Synopsis: ssh-client USERNAME HOSTNAME PORT COMMAND"

ssh login host port command = do
  initialize True
  home <- getEnv "HOME"
  let known_hosts = home </> ".ssh" </> "known_hosts"
      public = home </> ".ssh" </> "id_rsa.pub"
      private = home </> ".ssh" </> "id_rsa"
  withSession host port $ \session -> do
    r <- checkHost session host port known_hosts
    publicKeyAuthFile session login public private ""
    (Just ch, !src) <- execCommand True session command
    hSetBuffering stdout NoBuffering
    src =$= C.decodeUtf8 =$= C.linesUnbounded $$ CL.mapM_ TIO.putStrLn
    rc <- getReturnCode ch
    putStrLn $ "Exit code: " ++ show rc
  exit

returnStrict !x = return x
