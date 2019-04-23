{-# LANGUAGE OverloadedStrings #-}
module Main where

import Database.Dpi (
    ExecMode(ModeExecDefault)
  , defaultOracle, withContext, withConnection, withStatement
  , executeStatement, fetch, getQueryValue
  )


main :: IO ()
main = putStrLn "Hello, Haskell!"


repl :: IO ()
repl = do
  let conf = defaultOracle "KB" "KB123456" "10.132.37.241:1521/EDMP"
  withContext $ \ctx ->
    withConnection ctx conf return $ \conn ->
      withStatement conn False "select sysdate from dual" $ \st -> do
        r <- executeStatement st ModeExecDefault
        f <- fetch st
        mapM (getQueryValue st) [1..r] >>= print
-- >>> conn <- connectOracle "KB" "KB123456" "10.132.37.241:1521/EDMP"
-- >>>

