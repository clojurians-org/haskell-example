module Main where

import Database.HDBC (
    IConnection(
        dbServerVer, proxiedClientName, dbTransactionSupport
      , run, commit, disconnect
      )
  , SqlValue(..), toSql, fromSql
  , Statement(describeResult, execute, executeMany)
  , SqlError(..)
  , prepare, withTransaction, quickQuery', fetchAllRowsAL
  , getTables, describeTable
  )

import Database.Oracle

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- >>> :reload

