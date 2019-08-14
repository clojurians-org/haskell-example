{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}

module Backend.Streaming where

import Common.Types

import Data.Conduit (ConduitT, runConduit, bracketP, yield, (.|))
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T

import Data.Functor ((<&>))
import Control.Monad.Loops (iterateWhile)
import Data.String.Conversions (cs)

import qualified Database.Dpi as Oracle
import Control.Monad.Trans.Resource (MonadResource, allocate, release, runResourceT)
import Control.Monad.Trans (MonadIO(liftIO))
import Control.Exception (finally)
import Control.Monad (when, void)
import UnliftIO.STM as U
import UnliftIO.Async as U
import Control.Monad.IO.Unlift (MonadUnliftIO)

import Control.Concurrent.STM.TBMChan (TBMChan, newTBMChanIO, closeTBMChan, writeTBMChan, readTBMChan)

tshow :: (Show a) => a -> T.Text
tshow = cs . show

bracketR :: forall a b c m.MonadResource m
  => IO a -> (a -> IO c) -> (a -> m b) -> m b
bracketR alloc free inside = do
  (key, seed) <- allocate alloc (void . free)
  res <- inside seed
  release key
  return res
  
oracleChan :: forall m. (MonadResource m, MonadIO m, MonadUnliftIO m)
  => Credential -> T.Text -> T.Text -> m (TBMChan [Oracle.DataValue])
oracleChan (Credential hostName hostPort username password) database sql = do
  let config = Oracle.defaultOracle (cs username) (cs password)
                 (cs (hostName <> ":" <> tshow hostPort <> "/" <> database))
      defCursorSize = 2000
      defBatchSize = 20000
  (reg, chan) <- allocate (newTBMChanIO defBatchSize) (U.atomically . closeTBMChan)
  --U.async $
  bracketR Oracle.createContext Oracle.destroyContext $ \ctx ->
    bracketR (Oracle.createConnection ctx config return)
             (\c -> Oracle.closeConnection Oracle.ModeConnCloseDefault c
                     `finally` Oracle.releaseConnection c) $ \conn ->
      bracketR (Oracle.prepareStatement conn False (cs sql)) Oracle.releaseStatement $ \stmt -> liftIO $ do
          Oracle.setFetchArraySize stmt defCursorSize
          Oracle.executeStatement stmt Oracle.ModeExecDefault
          sinkRows chan stmt defCursorSize
          release reg
  return chan
  where
    sinkRows chan stmt cursorSize = do
      (more, rows) <- Oracle.fetchRows stmt cursorSize
      atomically $ writeTBMChan chan rows
      when more $ sinkRows chan stmt cursorSize


myRepl :: IO ()
myRepl = do
  runResourceT $ do
    chan <- oracleChan (credential "10.132.37.241:1521" "KB" "KB123456") "EDMP" "select * from tb_interface where rownum <= 2"
    vs <- U.atomically (readTBMChan chan)
    liftIO $ print vs
    liftIO $ putStrLn "finished!"