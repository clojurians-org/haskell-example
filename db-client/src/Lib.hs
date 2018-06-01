{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( 
    ) where

import Control.Exception (bracket)
import Database.MySQL.Base (ConnectInfo(..), Connection, defaultConnectInfo, connect, close)
import Database.MySQL.Base.Types (Field)
import Database.MySQL.Simple (Query(..), query_, execute_, Only)

import qualified Database.HDBC as H
import qualified Database.HDBC.ODBC as HO
import Database.HDBC.ODBC (connectODBC)


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

oracleConnInfo = "Driver={MyOracle};DBQ=(DESCRIPTION=(ADDRESS_LIST=(ADDRESS=(PROTOCOL=TCP)(HOST=10.129.35.238)(PORT=1521)))(CONNECT_DATA=(SERVICE_NAME=EDWDBUAT)(SERVER=DEDICATED)));UID=fsd;PWD=fsd;"

oracleConn :: IO HO.Connection
oracleConn = connectODBC oracleConnInfo
