{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( 
    ) where

import Control.Exception (bracket)
import Database.MySQL.Base (ConnectInfo(..), Connection, defaultConnectInfo, connect, close)
import Database.MySQL.Base.Types (Field)
import Database.MySQL.Simple (Query(..), query_, execute_, Only)

import Database.HDBC (quickQuery, disconnect, SqlValue, commit)
import Database.HDBC.ODBC (connectODBC)
import qualified Database.HDBC.ODBC as HO


mysqlConnInfo = defaultConnectInfo {
  connectHost = "10.132.37.54"
, connectDatabase = "py"
, connectUser = "root"
, connectPassword = "mysql"
}

query1 :: IO Connection -> IO [Only Int]
query1 mysqlConn = bracket  (connect mysqlConnInfo) close $ \conn ->
  query_ conn "select 1 + 1"

query2 :: IO Connection -> IO [(Int, String)]
query2 mysqlConn = bracket  (connect mysqlConnInfo) close $ \conn ->
  query_ conn "select 1 + 1, 'a'"

oracleConnInfo = "Driver={MyOracle};DBQ=(DESCRIPTION=(ADDRESS_LIST=(ADDRESS=(PROTOCOL=TCP)(HOST=10.129.35.238)(PORT=1521)))(CONNECT_DATA=(SERVICE_NAME=EDWDBUAT)(SERVER=DEDICATED)));UID=fsd;PWD=fsd;"


oraQuery1 :: IO [[SqlValue]]
oraQuery1 = bracket (connectODBC oracleConnInfo) (\x -> do {commit x; putStrLn "hello"; disconnect x}) $ \conn ->
  quickQuery conn "select 1 + 1 as a, 2 + 2 as b from dual" []


