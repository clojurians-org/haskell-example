{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}

module Backend.Conduit where

import Common.Types

import Data.Conduit (ConduitT, runConduit, bracketP, yield, (.|))
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T

import qualified Database.Dpi as Oracle
import Control.Monad.Trans.Resource (MonadResource, allocate, release, runResourceT)
import Control.Monad.Trans (MonadIO)
import Control.Exception (finally)
import UnliftIO.STM as U

import Control.Concurrent.STM.TBMChan (TBMChan, newTBMChanIO, closeTBMChan, writeTBMChan)

bracketR :: MonadResource m
  => IO a -> (a -> IO ()) -> (a -> m b) -> m b
bracketR alloc free inside = do
  (key, seed) <- allocate alloc free
  res <- inside seed
  release key
  return res
  
oracleChan :: forall m a. (MonadResource m, MonadIO m, Show a)
  => Credential -> T.Text -> T.Text -> m (TBMChan [a])
oracleChan (Credential hostName hostPort username password) database sql = do
  let config = undefined
      sql = ""
      defBatchSize = 2000
  (reg, chan) <- allocate (newTBMChanIO defBatchSize) (U.atomically . closeTBMChan)
  bracketR Oracle.createContext Oracle.destroyContext $ \ctx ->
      undefined
    {--
    bracketP (Oracle.createConnection ctx config return)
             (\c -> Oracle.closeConnection Oracle.ModeConnCloseDefault c
                     `finally` Oracle.releaseConnection c) $ \conn ->
      bracketP (Oracle.prepareStatement conn False sql) Oracle.releaseStatement $ \stmt -> do
        -- r <- Oracle.executeStatement st Oracle.ModeExecDefault
    --}
--  undefined
--        yieldMany ["a", "b"]
