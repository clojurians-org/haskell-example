{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import Lib
import GHC.Generics (Generic)
import Control.Monad (forM_)

import Data.Proxy as X (Proxy(Proxy))
import Network.HTTP.Client as H (Proxy(Proxy))
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Map (Map, singleton)
import Data.List (stripPrefix)
import Data.List.Split (chunksOf)

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy as BL (concat, toStrict)
import Data.ByteString.Lazy.Char8 as BLC (unlines)
import Data.ByteString.Lazy.UTF8 as BLU (toString)
import Data.Aeson (
    ToJSON(toJSON), encode
  , genericToJSON, defaultOptions
  , fieldLabelModifier
  )

import Network.HTTP.Client (
    Manager
  , Request (method, host, port, path, requestBody, requestHeaders)
  , RequestBody(RequestBodyLBS, RequestBodyBS)
  , Response(responseStatus)
  , httpLbs
  , newManager
  , defaultManagerSettings
  , managerSetProxy
  , proxyEnvironment
  -- request
  , defaultRequest
  )
-- import Network.HTTP.Client.TLS


import Test.QuickCheck (quickCheck)
import Test.QuickCheck.Arbitrary (Arbitrary(arbitrary))
import Test.QuickCheck.Gen (Gen, generate, sample, choose, vectorOf)

import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)

(=:) = singleton

class ToNdJSON a where
  toNdJson :: [a] -> ByteString

data EsTestData = EsTestData {
    ed_userName :: String
  , ed_password :: String
  , ed_email :: String
  , ed_type :: Int
  } deriving (Show, Generic)
instance ToJSON EsTestData where
  toJSON = genericToJSON defaultOptions {
      fieldLabelModifier = fromJust . stripPrefix "ed_"
    }

instance Arbitrary EsTestData where
  arbitrary = EsTestData <$> vectorOf 100 arbitrary
                           <*> vectorOf 100 arbitrary
                           <*> vectorOf 100 arbitrary
                           <*> choose (0, 9)

data EsTestRecord = EsTestRecord {
    er_index :: String
  , er_id :: String
  , er_data :: EsTestData
  } deriving (Show, Generic)

-- {"index" : {"_index" : "larluo", "_type": "_doc", "_id" : "1"}}                           
instance ToNdJSON EsTestRecord where
  toNdJson xs = BLC.unlines (concatMap mkInsert xs)
    where
      mkIndexPart :: EsTestRecord -> Map String (Map String String)
      mkIndexPart (EsTestRecord index id _) =
        "index" =: ( "_index" =: index
                     <> "_type" =: "_doc"
                     <> "_id" =: id
                     )
      mkInsert x = [
          encode (mkIndexPart x)
        , encode (er_data x)
        ]

-- >>> :reload
-- Ok, two modules loaded.
-- >>> recs <- mkRecords "larluo" 10
-- >>> toNdJson recs

mkManager :: IO Manager
mkManager = do
  let proxyEnv = proxyEnvironment $ Just (H.Proxy "127.0.0.1" 8118)
  let settings = managerSetProxy proxyEnv defaultManagerSettings
  newManager settings

mkRecords :: String -> [Int] -> Int -> IO [[EsTestRecord]]
mkRecords index rng batch = do
  esDatas <- mkEsDatas
  return $ chunksOf batch $ zipWith (\esData i -> EsTestRecord index (show i) esData) esDatas rng
    where mkEsDatas = generate $ vectorOf (length rng) (arbitrary :: Gen EsTestData) 

putES :: Manager -> ByteString -> [EsTestRecord] -> IO ()
putES mgr host recs = do
  resp <- flip httpLbs mgr defaultRequest {
      host = BL.toStrict host
    , port = 9200
    , path = "_bulk"
    , method = "POST"
    , requestHeaders = [ ("Content-Type", "application/x-ndjson")
                       , ("Authorization", "Basic ZWxhc3RpYzpTaHJiYW5rQDIwMTU=")]
    , requestBody = RequestBodyLBS (toNdJson recs)
    }
  putStrLn . show $ responseStatus resp

getES :: Manager -> ByteString -> ByteString -> ByteString -> IO ()
getES mgr host index key = do
  resp <- flip httpLbs mgr defaultRequest {
      host = BL.toStrict host
    , port = 9200
    , path = BL.toStrict $ BL.concat ["/", index, "/_doc/", key]
    }
  putStrLn . show $ responseStatus resp

main :: IO ()
main = do
  [host, index, startRow, endRow] <- getArgs
  putStrLn (show [host, index, startRow, endRow])
  startTS <- getCurrentTime
  mgr <- mkManager
  recs <- mkRecords index [(read startRow :: Int) .. (read endRow :: Int)] 1000
  forM_ recs $ putES mgr (fromString host)
  endTS <- getCurrentTime
  putStrLn $ show (endTS `diffUTCTime` startTS) <> " elapsed."


-- >>> main 


-- >>> getES mgr "10.132.37.201" "larluo2" "1"

-- curl -XDELETE 10.132.37.201:9200/larluo
-- curl 10.132.37.201:9200/_cat/indices/larluo
-- curl 10.132.37.201:9200/larluo/_count


-- >>> 1 + 2 * 5
-- 11
