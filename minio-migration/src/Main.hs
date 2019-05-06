module Main where

import Control.Monad.IO.Class (liftIO) 
import Data.Function ((&))
import Control.Monad (void, when)
import Network.Minio (
    ConnectInfo(..), Credentials(..)
  , setCreds, setRegion
  , runMinio, makeBucket, bucketExists, listBuckets
  , listObjects
  , fPutObject, defaultPutObjectOptions
  )

import Conduit (runConduit, (.|), takeC, mapM_C)


myCI :: ConnectInfo
myCI = "http://10.132.34.83:9000"
         & setCreds (Credentials "BY2BFHISRTPNY36IR4TD"
                                 "ZB66/2jxW0bXkiEU0kufFT0ni1tOut9QJG8v1hb7")
         & setRegion "us-east-1"
  
main :: IO ()
main = do
  res <- runMinio myCI $ do
    liftIO $ putStrLn $ "start ..."
    {--
    buckets <- listBuckets
    liftIO $ print $ "Top 5 buckets: " ++ show (take 5 buckets)
    bExist <- bucketExists "larluo"
    when (not bExist) $ void $ makeBucket "larluo" Nothing
    liftIO $ putStrLn $ "--> put tgz.nix-2.2.2"

    fPutObject "larluo"
      "tgz.nix-2.2.2"
      "/home/op/my-env/nix.sh.out/tgz.nix-2.2.2"
      defaultPutObjectOptions
    --}
    runConduit $ listObjects "icif.uat" Nothing True
              .| takeC 10
              .| mapM_C (liftIO . print)
  case res of
    Left e -> putStrLn $ "operate failed due to " ++ (show e)
    Right _ -> putStrLn "done!"

-- >>> main
-- start ...
-- put tgz.nix-2.2.2
-- done!
