{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module Backend.Streaming where

import Common.Types

import Data.Conduit (ConduitT, runConduit, runConduitRes, bracketP, yield, (.|))
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T

import Data.Functor ((<&>))
import Control.Monad.Loops (iterateWhile)
import Data.String.Conversions (cs)

import qualified Database.Dpi as Oracle
import qualified Database.Dpi.Field as Oracle

import Control.Monad.Trans.Resource (MonadResource, allocate, release, runResourceT)
import Control.Monad.Trans (MonadIO(liftIO), MonadTrans(lift))
import Control.Exception (finally, catch, SomeException(..))
import Control.Monad (when, void)
import UnliftIO.STM as U
import UnliftIO.Async as U
import Control.Monad.IO.Unlift (MonadUnliftIO)

import Data.Maybe (isJust, fromMaybe)
import Control.Concurrent.STM.TBMChan (TBMChan, newTBMChanIO, closeTBMChan, writeTBMChan, readTBMChan)

import Data.Conduit.TMChan (sourceTBMChan)

import qualified Data.ByteString as B
import Text.Heredoc (str)

import Labels 

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
  => Credential -> T.Text -> T.Text -> m (TBMChan [Oracle.DataField])
oracleChan (Credential hostName hostPort username password) database sql = do
  let config = Oracle.defaultOracle (cs username) (cs password)
                 (cs (hostName <> ":" <> tshow hostPort <> "/" <> database))
      defCursorSize = 2000
      defBatchSize = 20000
  (reg, chan) <- allocate (newTBMChanIO defBatchSize) (U.atomically . closeTBMChan)
  U.async $ bracketR Oracle.createContext Oracle.destroyContext $ \ctx ->
    bracketR (Oracle.createConnection ctx config return)
             (\c -> Oracle.closeConnection Oracle.ModeConnCloseDefault c
                     `finally` Oracle.releaseConnection c) $ \conn ->
      bracketR (Oracle.prepareStatement conn False (cs sql)) Oracle.releaseStatement $ \stmt -> liftIO $ do
          Oracle.setFetchArraySize stmt defCursorSize
          cn <- Oracle.executeStatement stmt Oracle.ModeExecDefault
                `catch` \(SomeException e) -> print e >> fail (show e)
          sinkRows chan stmt cn defCursorSize 
          release reg
  return chan
  where
    -- https://github.com/oracle/odpi/issues/70    
    sinkRows chan stmt cn cursorSize = do
      offset <- Oracle.fetch stmt 
      when (isJust offset)  $ do 
        row <- mapM (\i -> Oracle.DataField <$> Oracle.getQueryInfo stmt i
                                            <*> Oracle.getQueryValue stmt i)
                  [1..cn]
        atomically $ writeTBMChan chan row
        sinkRows chan stmt cn cursorSize


textOracle :: (MonadIO m) => Oracle.DataField -> m T.Text
textOracle v = liftIO (Oracle.fromDataField v) <&> maybe "" (cs :: B.ByteString -> T.Text)

oracleShowTables ::  forall m. (MonadIO m, MonadUnliftIO m)
  => Credential -> T.Text -> m [("schema" := T.Text, "table" := T.Text)]
oracleShowTables credential database =
  runConduitRes $ (lift chan >>= sourceTBMChan) .| C.mapM (liftIO . labelOracle) .| C.sinkList
  where
    chan = oracleChan credential database sql
    sql = [str| select owner, table_name
              | from all_tables
              | where tablespace_name not in ('SYSTEM', 'SYSAUX')
              |]
    labelOracle xs = (,)
      <$> fmap (#schema := ) (textOracle (xs !! 0))
      <*> fmap (#table :=) (textOracle (xs !! 1))

oracleDescribeTable :: forall m. (MonadIO m, MonadUnliftIO m)
  => Credential -> T.Text -> (T.Text, T.Text) -> m [("name" := T.Text, "type" := T.Text, "desc" := T.Text)]
oracleDescribeTable credential database (schema, table) = do
  liftIO $ print sql
  runConduitRes $ (lift chan >>= sourceTBMChan) .| C.mapM (liftIO . labelOracle) .| C.sinkList
  where
    chan = oracleChan credential database sql
    sql = "select t1.column_name, t1.data_type, t2.comments from all_tab_columns t1 inner join all_col_comments t2"
          <> "  on t1.owner = t2.owner and t1.table_name = t2.table_name and t1.column_name = t2.column_name"
          <> "  where t1.owner = '" <> schema <> "' AND t1.table_name = '" <> table <> "'"
    labelOracle xs = (,,)
      <$> fmap (#name :=) (textOracle (xs !! 0))
      <*> fmap (#type :=) (textOracle (xs !! 1))
      <*> fmap (#desc :=) (textOracle (xs !! 2))    
    

myRepl :: IO ()
myRepl = do
  -- oracleShowTables (credential "10.132.37.241:1521" "KB" "KB123456") "EDMP" >>= print
  oracleDescribeTable (credential "10.132.37.241:1521" "KB" "KB123456") "EDMP" ("KB", "TB_INTERFACE_LOG")
    >>= mapM_ print
  
