{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad.IO.Class (liftIO) 
import Data.Function ((&))
import Control.Monad (void, when)
import Network.Minio (
    Minio, ConnectInfo(..), Credentials(..), ObjectInfo(..)
  , setCreds, setRegion
  , runMinio, makeBucket, bucketExists, listBuckets
  , listObjects, listObjectsV1, fPutObject, fGetObject
  , defaultGetObjectOptions, defaultPutObjectOptions
  )

import Criterion.Measurement (getCPUTime, getTime, secs)


import qualified Data.Text.IO as T
import Data.String.Conv (toS)

import UnliftIO.Concurrent (forkIO, threadDelay)
import UnliftIO.STM (atomically)
import UnliftIO.Resource (runResourceT)
import UnliftIO.Async (async)

import Control.Monad.Trans.Resource (allocate, release, register, liftResourceT)
import Control.Concurrent.STM.TBMChan (newTBMChan, newTBMChanIO, closeTBMChan)
import Conduit (runConduit, runConduitRes, ($$), (.|), takeC, mapC, mapM_C, lengthC, iterMC)
import qualified Data.Conduit.List as CL
import Data.Conduit.TMChan (sourceTBMChan, sinkTBMChan)

import Data.Conduit.Binary (sinkFileCautious)

myUATCI :: ConnectInfo
myUATCI = "http://X.X.X.X:9000"
         & setCreds (Credentials "XXXX"
                                 "xxxx")
         & setRegion "us-east-1"

main :: IO ()
main = do
  [bucket, outPath] :: [String] <- return ["icif.uat", "/home/op/my-work/haskell-example/minio-migration/data"]

  startTs <- getTime
  putStrLn $ "start ..."
  
  res <- runMinio myUATCI $ do
      (reg, chan) <- liftResourceT $ allocate (newTBMChanIO 2000) (atomically . closeTBMChan)
      _ <- async $ do
        runConduit $ listObjectsV1 (toS bucket) Nothing False
--                  .| takeC 2000
--                  .| iterMC (liftIO . print)
                  .| sinkTBMChan chan
        release reg

      runConduit $ sourceTBMChan chan
                .| mapC (toS . oiObject)
                .| mapM_C (\fp -> do
                              fGetObject (toS bucket) fp (outPath <> "/" <> toS fp) defaultGetObjectOptions
                              liftIO $ putStrLn $ (outPath <> "/" <> toS fp) ++ " downloaded!"
                          )

  endTs <- liftIO getTime
  case res of
    Left e -> putStrLn $ "operate failed due to " ++ (show e)
    Right _ -> putStrLn $ "cost: " ++ (secs (endTs - startTs))

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

-- >>> main
-- start ...
-- ObjectInfo {oiObject = "131181198911061075.jpg", oiModTime = 2017-05-13 09:00:16.412 UTC, oiETag = "\"a4aec9ccb10631c35485d7ca259994c4\"", oiSize = 7654, oiMetadata = fromList []}
-- cost: 6.194 s
 
