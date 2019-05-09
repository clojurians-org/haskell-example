{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude
import System.Environment (getArgs)
import Data.Function ((&))
import Data.String (fromString)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO) 

import Network.Minio (
    Minio, ConnectInfo(..), Credentials(..), ObjectInfo(..), Bucket, Object
  , setCreds, setRegion
  , runMinio, makeBucket, bucketExists, listBuckets
  , listObjects, listObjectsV1, fPutObject, fGetObject
  , defaultGetObjectOptions, defaultPutObjectOptions
  )

import Criterion.Measurement (getCPUTime, getTime, secs)

import Data.Text(Text)
import qualified Data.Text.IO as T
import Data.String.Conv (toS)

import UnliftIO.Concurrent (forkIO, threadDelay)
import UnliftIO.STM (atomically)
import UnliftIO.Resource (runResourceT)
import UnliftIO.Async (async)
import UnliftIO.Directory (
  createDirectoryIfMissing, doesFileExist
  )
import  UnliftIO.Exception (SomeException, catch)

import Control.Monad.Trans.Resource (allocate, release, register, liftResourceT)
import Control.Concurrent.STM.TBMChan (newTBMChan, newTBMChanIO, closeTBMChan)
import Conduit (runConduit, runConduitRes, ($$), (.|), takeC, mapC, mapM_C, lengthC, iterMC)
import qualified Data.Conduit.List as CL
import Data.Conduit.TMChan (sourceTBMChan, sinkTBMChan)

import Data.Default (def)
import Control.Retry (retrying)
import Data.Maybe (isNothing)

importMinioMissing :: FilePath -> Bucket -> ObjectInfo -> Minio ()
importMinioMissing localDir bucket remoteOI = do
  let remotePathText = oiObject remoteOI
  let localPath = localDir <> "/" <> (toS remotePathText)
  let getObjectMaybe = fmap Just (fGetObject bucket remotePathText localPath defaultGetObjectOptions)  
                         `catch` \(e ::SomeException) -> liftIO (putStrLn (show e)) >> return Nothing
  fileExist <- doesFileExist localPath 
  if | fileExist -> liftIO $ putStrLn $ localPath ++ " exist already!"
     | otherwise -> retrying def (const $ return . isNothing) (const getObjectMaybe) >> (liftIO $ putStrLn $ localPath ++ " downloaded!")

mkCI :: ConnectInfo -> Text -> Text -> ConnectInfo
mkCI ci accessKey secretKey= ci & setCreds (Credentials accessKey secretKey) & setRegion "us-east-1"

main :: IO ()
main = do
  [outPath, host, accessKey, secretKey, bucket] :: [String] <- getArgs
--      return ["/home/op/my-work/haskell-example/minio-migration/data", "http://10.132.81.38:9000", "BY2BFHISRTPNY36IR4TD", "ZB66/2jxW0bXkiEU0kufFT0ni1tOut9QJG8v1hb7", "icif.uat"]
--      return ["/home/op/my-work/haskell-example/minio-migration/data", "http://10.129.35.175:9000", "BY2BFHISRTPNY36IR4TD", "ZB66/2jxW0bXkiEU0kufFT0ni1tOut9QJG8v1hb7", "cib"]
--      return ["/home/op/my-work/haskell-example/minio-migration/data", "http://10.129.35.175:9000", "BY2BFHISRTPNY36IR4TD", "ZB66/2jxW0bXkiEU0kufFT0ni1tOut9QJG8v1hb7", "test.icif"]

  startTs <- getTime
  putStrLn $ "start ..."

  let outBucketPath = outPath <> "/" <> bucket
  createDirectoryIfMissing True outBucketPath
  res <- runMinio (mkCI (fromString host) (toS accessKey) (toS secretKey)) $ do
      (reg, chan) <- liftResourceT $ allocate (newTBMChanIO 2000) (atomically . closeTBMChan)
      _ <- async $ do
        runConduit $ listObjectsV1 (toS bucket) Nothing True
--                  .| takeC 10
--                  .| iterMC (liftIO . print)
                  .| sinkTBMChan chan
        release reg

      runConduit $ sourceTBMChan chan
--                  .| iterMC (liftIO . print)
                .| mapM_C (importMinioMissing outBucketPath (toS bucket))

  endTs <- liftIO getTime
  case res of
    Left e -> putStrLn $ "operate failed due to " ++ (show e)
    Right _ -> putStrLn $ "cost: " ++ (secs (endTs - startTs))

repl :: IO ()
repl = do
  -- res <- runMinio (mkCI "http://10.132.81.38:9000" "BY2BFHISRTPNY36IR4TD" "ZB66/2jxW0bXkiEU0kufFT0ni1tOut9QJG8v1hb7") $ do
  res <- runMinio (mkCI "http://10.129.35.175:9000" "BY2BFHISRTPNY36IR4TD" "ZB66/2jxW0bXkiEU0kufFT0ni1tOut9QJG8v1hb7") $ do
    cnt <- runConduit $ listObjectsV1 "test.icif" Nothing True
                     .| lengthC
    liftIO $ putStrLn $ "count:" ++ (show cnt)
  putStrLn (show res)

test :: Minio ()
test = do
    threadDelay (1000 * 1000 * 5)
    buckets <- listBuckets
    liftIO $ print $ "Top 5 buckets: " ++ show (take 5 buckets)
    bExist <- bucketExists "larluo"
    when (not bExist) $ void $ makeBucket "larluo" Nothing
    liftIO $ putStrLn $ "--> put tgz.nix-2.2.2"

    fPutObject "larluo"
      "tgz.nix-2.2.2"
      "/home/op/my-env/nix.sh.out/tgz.nix-2.2.2"
      defaultPutObjectOptions
    cnt <- runConduit $ listObjectsV1 "icif.uat" Nothing False
                     .| lengthC
    liftIO $ putStrLn $ "count:" ++ (show cnt)


-- >>> main
-- start ...
-- ObjectInfo {oiObject = "131181198911061075.jpg", oiModTime = 2017-05-13 09:00:16.412 UTC, oiETag = "\"a4aec9ccb10631c35485d7ca259994c4\"", oiSize = 7654, oiMetadata = fromList []}
-- cost: 6.194 s
 
