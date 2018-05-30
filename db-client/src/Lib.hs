{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( 
    ) where

import Control.Exception (bracket)
import Database.MySQL.Base (ConnectInfo(..), Connection, defaultConnectInfo, connect, close)
import Database.MySQL.Simple (Query(..), query_, execute_, Only)


mysqlConnInfo = defaultConnectInfo {
  connectHost = "10.132.37.54"
, connectDatabase = "py"
, connectUser = "root"
, connectPassword = "mysql"
}

mysqlConn :: IO Connection
mysqlConn = connect mysqlConnInfo


query1 :: IO Connection -> IO [Only Int]
query1 mysqlConn = bracket  mysqlConn close $ \conn ->
  query_ conn "select 1 + 1"

query2 :: IO Connection -> IO [(Int, String)]
query2 mysqlConn = bracket  mysqlConn close $ \conn ->
  query_ conn "select 1 + 1, 'a'"

