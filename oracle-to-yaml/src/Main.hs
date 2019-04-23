{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Loops (unfoldM)
import Database.Dpi (
    ExecMode(ModeExecDefault), DataValue(..), OracleConfig, SQL
  , defaultOracle, withContext, withConnection, withStatement
  , executeStatement, getRowCount, fetch, getQueryValue, getQueryInfo
  )
import Database.Dpi.Field (
    DataField(..), FromDataFields(fromDataFields')
  )

main :: IO ()
main = putStrLn "Hello, Haskell!"


quickQuery :: FromDataFields a => OracleConfig -> SQL -> IO [a]
quickQuery conf sql = do
  let fetchRow st cn = mapM (\i -> DataField <$> (getQueryInfo st i) <*> (getQueryValue st i)) [1..cn]
  withContext $ \ctx ->
    withConnection ctx conf return $ \conn ->
      withStatement conn False sql $ \st -> do
        cn <- executeStatement st ModeExecDefault
        allFetch <- unfoldM (fetch st)
        mapM (const (fetchRow st cn >>= fromDataFields')) allFetch
        

-- >>> :set -XOverloadedStrings
-- >>> :reload

-- >>> let dbConf = defaultOracle "KB" "KB123456" "10.132.37.241:1521/EDMP"
-- >>> quickQuery dbConf "select * from tb_interface where rownum <= 2" :: IO [String]
