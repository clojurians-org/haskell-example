{-# LANGUAGE OverloadedStrings #-}

module TryRedis where

import Data.Pool
import Database.Redis
import Control.Monad.IO.Class  (liftIO)
import Data.ByteString.Char8 (pack)

connectionInfo :: ConnectInfo
connectionInfo = defaultConnectInfo {
  connectHost           = "localhost",
  connectPort           = PortNumber 6379, -- Redis default port
  connectAuth           = Nothing,         -- No password
  connectDatabase       = 0,               -- SELECT database 0
  connectMaxConnections = 50,              -- Up to 50 connections
  connectMaxIdleTime    = 30,              -- Keep open for 30 seconds
  connectTimeout        = Nothing,         -- Don't add timeout logic
  connectTLSParams      = Nothing         -- Do not use TLS
}

createRedisPool :: IO (Pool Connection)
createRedisPool = createPool newConn delConn 1 10 5
  where
    newConn :: IO Connection
    newConn = connect connectionInfo
    delConn :: Connection -> IO ()
    delConn conn = disconnect conn

-- 设置例子
action1 :: IO ()
action1 = do
  pool <- createRedisPool
  withResource pool $ \conn -> do
    runRedis conn $ do
      er <- mset [("hedis","haskell"), ("hedis1","hello")]
      case er of
        Right a -> liftIO $ print a
        Left e -> liftIO $ print e

-- 取值例子
action2 :: IO ()
action2 = do
  pool <- createRedisPool
  withResource pool $ \conn -> do
    runRedis conn $ do
      er <- mget ["hedis","hedis1"]
      case er of
        Right a -> liftIO $ print a
        Left e -> liftIO $ print e


-- 尝试放结构,好像没法放，只能是序列化成字符串后放
data User = User {
  name :: String,
  age :: Int
} deriving (Show)

action3 :: IO ()
action3 = do
  pool <- createRedisPool
  withResource pool $ \conn -> do
    runRedis conn $ do
      er <- mset [("user", pack $ show $ User "jacky" 20 )]
      case er of
        Right a -> liftIO $ print a
        Left e -> liftIO $ print e
