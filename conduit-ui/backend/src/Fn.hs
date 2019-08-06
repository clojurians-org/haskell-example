{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Fn where

import Prelude
import GHC.Word (Word16)
import Data.Either.Combinators (fromRight, isLeft, fromRight')
import Data.Function ((&))
import Control.Applicative ((<|>))
import Control.Monad (when, forM_, mapM_, void)

import qualified Data.ByteString as B
import Data.String.Conversions (cs)
import Text.Heredoc (str)

import Data.Conduit ( ConduitT, (.|), ZipConduit(..), getZipConduit
                    , runConduit, runConduitRes
                    , bracketP, mergeSource, yield)
import qualified Data.Conduit.Combinators as C

import qualified Data.Aeson as J (decode, eitherDecode, encode)
import Labels ((:=)(..))
import qualified Labels as L
import Labels.JSON ()

import qualified Hasql.Connection as H (Connection, Settings, settings, acquire, settings)
import qualified Hasql.Session as H (Session(..), QueryError, run, statement)
import qualified Hasql.Statement as HS (Statement(..))
import qualified Hasql.Encoders as HE (Params(..), unit)
import qualified Hasql.Decoders as HD (Result(..), Row(..), unit, rowList, column, text)

import Control.Monad.Reader (runReaderT, ReaderT, ask)
import Control.Monad.Trans.Resource (ResourceT, allocate, release, runResourceT)
import Control.Concurrent.STM.TBMChan (TBMChan, newTBMChanIO, closeTBMChan, writeTBMChan)
import Data.Conduit.TMChan (sourceTBMChan, sinkTBMChan)

import UnliftIO.STM (atomically)
import UnliftIO.Async (async)

import qualified Transient.Base as TS (keep, async) 
import Control.Monad.Trans (MonadIO, MonadTrans, lift, liftIO)

import qualified Network.Minio as M
import qualified Network.Minio.S3API as M
import qualified Network.SSH.Client.LibSSH2 as SSH
import qualified Network.SSH.Client.LibSSH2.Foreign as SSH

import Control.Lens

unitSession :: B.ByteString -> H.Session ()
unitSession sql = H.statement () $ HS.Statement sql HE.unit HD.unit True

declareCursor :: B.ByteString -> B.ByteString -> params -> HE.Params params -> H.Session ()
declareCursor name sql params paramsEncoder =
  H.statement params $ HS.Statement sql' paramsEncoder HD.unit False
  where sql' =  "DECLARE " <> name <> " NO SCROLL CURSOR FOR " <> sql

closeCursor :: B.ByteString -> H.Session ()
closeCursor name = unitSession ("CLOSE " <> name)

fetchFromCursor :: B.ByteString -> Int -> HD.Result result -> H.Session result
fetchFromCursor name batchSize decoder =
  H.statement () $ HS.Statement sql HE.unit decoder True
  where sql = "FETCH FORWARD " <> (cs (show batchSize)) <> " FROM " <> name


pgToChan :: (Show a) => H.Connection -> B.ByteString -> B.ByteString -> Int -> Int -> HD.Row a
                        -> ResourceT IO (TBMChan [a]) 
pgToChan connection sql cursorName cursorSize chanSize rowDecoder = do
  let runS :: H.Session a -> IO (Either H.QueryError a)
      runS = flip H.run connection

  let sinkRows chan = do
        rowsE <- runS $ fetchFromCursor cursorName cursorSize (HD.rowList rowDecoder)
        when (isLeft rowsE) $ print rowsE
        let rows = fromRight' rowsE
        when ((not . null) rows) $ do
          atomically $ writeTBMChan chan rows
          sinkRows chan

--  runResourceT $ do
  (reg, chan) <- allocate (newTBMChanIO chanSize) (atomically . closeTBMChan)
  _ <- async $ liftIO $ do
        runS $ unitSession "BEGIN"
        runS $ declareCursor cursorName sql () HE.unit
        sinkRows chan
        runS $ closeCursor cursorName
        runS $ unitSession "COMMIT"
        release reg
  return chan

repl :: IO ()
repl = do
  let
    pgChan = do
      let
        sql = [str|select
                    |  id, name, description, 'type'
                    |, state, timeliness, params, result_plugin_type
                    |, vendor_id, server_id, success_code
                    |from tb_interface
                    |] :: B.ByteString
        
        pgSettings = H.settings "10.132.37.200" 5432 "monitor" "monitor" "monitor"
        (curName, cursorSize, chanSize) = ("larluo", 200, 1000)
        
        textColumn = HD.column HD.text
        mkRow = (,,,,,,,,,,)
                    <$> fmap (#id :=) textColumn
                    <*> fmap (#name :=) textColumn
                    <*> fmap (#description :=) textColumn
                    <*> fmap (#type :=) textColumn
                    <*> fmap (#state :=) textColumn
                    <*> fmap (#timeliness :=) textColumn
                    <*> fmap (#params :=) textColumn
                    <*> fmap (#result_plugin_type :=) textColumn
                    <*> fmap (#vendor_id :=) textColumn
                    <*> fmap (#server_id :=) textColumn
                    <*> fmap (#success_code :=) textColumn
                          
      chan <- do
        Right connection <- liftIO $ H.acquire pgSettings
        lift $ pgToChan connection sql curName cursorSize chanSize mkRow
      sourceTBMChan chan
    
    minIOSink = do
      let
        (ci', accessKey, secretKey, bucket, filepath)
          = ( "http://10.132.37.200:9000"
            , "XF90I4NV5E1ZC2ROPVVR"
            , "6IxTLFeA2g+pPuXu2J8BMvEUgywN6kr5Uckdf1O4"
            , "larluo"
            , "postgresql.txt")
        ci = ci' & M.setCreds (M.Credentials accessKey secretKey)
                 & M.setRegion "us-east-1"
      Right uid <- liftIO . M.runMinio ci $ do
        bExist <- M.bucketExists bucket
        when (not bExist) $ void $ M.makeBucket bucket Nothing
        M.newMultipartUpload bucket filepath  []

      C.chunksOfE (2000 * 1000)
        .| mergeSource (C.yieldMany [1..])
        .| C.mapM  (\(pn, v) -> liftIO . M.runMinio ci $ M.putObjectPart bucket filepath uid pn [] (M.PayloadBS v))
        .| (C.sinkList >>= yield . fromRight' . sequence)
        .| C.mapM (liftIO . M.runMinio ci . M.completeMultipartUpload bucket filepath uid)
        .| C.sinkNull
    sftpSink = do
      let (hostname, port, username, password) = ("10.132.37.201", 22, "op", "op")
          filepath = "aaa.txt"
          flags = [SSH.FXF_WRITE, SSH.FXF_CREAT, SSH.FXF_TRUNC, SSH.FXF_EXCL]
      bracketP (SSH.sessionInit hostname port) SSH.sessionClose $ \s -> do
        liftIO $ SSH.usernamePasswordAuth s username password
        bracketP (SSH.sftpInit s) SSH.sftpShutdown $ \sftp -> do
          bracketP (SSH.sftpOpenFile sftp filepath 0o777 flags) SSH.sftpCloseHandle $ \sftph ->
            C.mapM (liftIO . SSH.sftpWriteFileFromBS sftph) .| C.sinkNull

    myConduit = do
      void . runConduitRes . flip runReaderT dataSandbox $ do
        dataSandbox <- ask
        lift $ (L.get #dataSource dataSandbox )
          .| C.concat
          .| C.take 3
          .| C.map ((<> "\n") .cs . J.encode)
          .| (L.get #dataService dataSandbox)
          .| C.sinkList
          
    dataSandbox = ( #dataSource := pgChan
                  , #stateContainer := undefined
                  , #dataService := getZipConduit (ZipConduit minIOSink <* ZipConduit sftpSink))
          
  myConduit
