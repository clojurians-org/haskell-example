module Extensions (
  sshGet
)  where 

import Network.SSH.Client.LibSSH2.Foreign (SftpFileTransferFlags(FXF_READ)
                                        , SftpAttributes(saFileSize)
                                        , sftpOpenFile, sftpCloseHandle, sftpFstat)
import Network.SSH.Client.LibSSH2.Types (Sftp, SftpHandle)
import Network.SSH.Client.LibSSH2 (withSSH2User, scpReceiveFile
                                 , withSFTPUser, sftpReadFileToHandler, sftpListDir)

import System.IO (readFile, writeFile,withFile
                 ,IOMode(WriteMode))
import System.Environment (getEnv)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Knob as K

import Control.Exception (bracket, handle, SomeException(SomeException))
import Data.Attoparsec.ByteString.Lazy (Parser)
import qualified Data.Attoparsec.ByteString.Lazy as P
import qualified Data.Attoparsec.Combinator as P

sshListFile :: String -> String -> String -> FilePath -> IO ByteString
sshListFile username password ip filepath = do
  putStrLn "sshListFile..."
  home <- getEnv "HOME"
  withSFTPUser (home ++ "/.ssh/known_hosts") username password ip 22 $ \sftp ->
    BC.pack . show . fmap fst <$> sftpListDir sftp "/home/larluo"

sshReadFile :: String -> String -> String -> FilePath -> IO ByteString
sshReadFile username password ip filepath = do
  home <- getEnv "HOME"
  withSFTPUser (home ++ "/.ssh/known_hosts") username password ip 22 $ \sftp ->
   bracket (sftpOpenFile sftp filepath 0 [FXF_READ]) sftpCloseHandle $ \sftph -> do
     fsize <- fromIntegral . saFileSize <$> sftpFstat sftph
     knob <- K.newKnob ""
     K.withFileHandle knob "[knob]" WriteMode $ \h -> do
       sftpReadFileToHandler sftph h fsize
     B.fromStrict <$> K.getContents knob

urlP :: Parser (String, String, String, String)
urlP = do
  username <- BC.unpack . B.pack <$> P.manyTill P.anyWord8 (P.string ":")
  password <- BC.unpack . B.pack <$> P.manyTill P.anyWord8 (P.string "@")
  ip <- BC.unpack . B.pack <$> P.manyTill P.anyWord8 (P.string ":")
  filepath <- BC.unpack . B.pack <$> P.many1 P.anyWord8
  return (username, password, ip, filepath)
  
sshGet :: ByteString -> IO (Either String ByteString)
sshGet url = do
  handle (\(SomeException e) -> (return . Left) ("sshGet_EXCEPTION: " ++ show e)) $
    case P.eitherResult (P.parse urlP url) of
      Right (username, password, ip, filepath) ->
        case last filepath of 
          '/' -> Right <$> sshListFile username password ip filepath
          _ -> Right <$> sshReadFile username password ip filepath
      Left x -> return (Left x)

repl :: IO ()
repl = do
  sshGet "larluo:LuoHao0402@localhost:/home/larluo/test.txt" >>= print
  sshGet "larluo:LuoHao0402@localhost:/home/larluo/" >>= print
  handle (\(SomeException _) -> putStrLn "Error calculating result") (print (5 `div` 0))
  undefined

