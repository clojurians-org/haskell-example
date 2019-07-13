{-# LANGUAGE BangPatterns #-}
module Main where

import Prelude 
import Data.Function ((&))
import Data.Either.Combinators (fromRight, isLeft, fromRight')
import Data.String.Conversions (cs)

import qualified Data.Aeson as J (decode, eitherDecode, encode)

import Text.Heredoc (str)

import qualified Data.ByteString as B
import Data.Text (Text)

import qualified Hasql.Connection as H (acquire, settings)
import qualified Hasql.Session as H (Session(..), run, statement)
import qualified Hasql.Transaction.Sessions as HT (IsolationLevel(..), Mode(..), transaction)
import qualified Hasql.Transaction as HT (Transaction(..), statement)
import Hasql.Statement (Statement(..))
import qualified Hasql.Encoders as HE (Params(..), unit)
import Hasql.Decoders (Row(..), singleRow, rowList, column, nullableColumn, int8, text)
import qualified Hasql.Decoders as HD (Result(..), unit)

import Labels ((:=)(..))
import Labels.JSON ()

import UnliftIO.STM (atomically)
import UnliftIO.Async (async)
import Control.Monad (join, when, forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Resource (allocate, release, runResourceT)
import Control.Concurrent.STM.TBMChan (newTBMChanIO, closeTBMChan, writeTBMChan)

import Data.Conduit ((.|), runConduit, yield)
import qualified Data.Conduit.Combinators as C (yieldMany)
import Data.Conduit.TMChan (sourceTBMChan, sinkTBMChan)

import Network.HTTP.Client
  (ManagerSettings(..), newManager, defaultManagerSettings, responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import qualified Network.Minio as M

main :: IO ()
main = undefined

unitStatement :: B.ByteString -> H.Session ()
unitStatement sql = H.statement () $ Statement sql HE.unit HD.unit True

declareCursor :: B.ByteString -> B.ByteString -> params -> HE.Params params -> H.Session ()
declareCursor name sql params paramsEncoder =
  H.statement params $ Statement sql' paramsEncoder HD.unit False
  where sql' =  "DECLARE " <> name <> " NO SCROLL CURSOR FOR " <> sql

closeCursor :: B.ByteString -> H.Session ()
closeCursor name = unitStatement ("CLOSE " <> name)

fetchFromCursor :: B.ByteString -> Int -> HD.Result result -> H.Session result
fetchFromCursor name batchSize decoder =
  H.statement () $ Statement sql HE.unit decoder True
  where sql = "FETCH FORWARD " <> (cs (show batchSize)) <> " FROM " <> name

repl :: IO ()
repl = do
  let sql = [str|select
                |  id, name, description, 'type'
                |, state, timeliness, params, result_plugin_type
                |, vendor_id, server_id, success_code
                |from tb_interface
                |] :: B.ByteString
  let (cursorName, batchSize, chanSize) = ("larluo", 10, 100) 
  let connectionSettings = H.settings "10.132.37.200" 5432 "monitor" "monitor" "monitor"

  Right connection <- H.acquire connectionSettings
  let runS = flip H.run connection 

  let sinkRows chan = do
        rowsE <- runS $ fetchFromCursor cursorName batchSize (rowList mkRow)
        when (isLeft rowsE) $ print rowsE
        let rows = fromRight' rowsE
        when ((not . null) rows) $ do
          atomically $ writeTBMChan chan rows
          sinkRows chan
        
  runResourceT $ do
    (reg, chan) <- allocate (newTBMChanIO chanSize) (atomically . closeTBMChan)
    _ <- async $ liftIO $ do
        runS $ unitStatement "BEGIN"
        runS $ declareCursor cursorName sql () HE.unit
        sinkRows chan
        runS $ closeCursor cursorName
        runS $ unitStatement "COMMIT"
        release reg

    let (ci, accessKey, secretKey, bucket)
          = ( "http://10.129.35.175:9000"
            , "BY2BFHISRTPNY36IR4TD"
            , "ZB66/2jxW0bXkiEU0kufFT0ni1tOut9QJG8v1hb7"
            , "larluo")            
    conn <- liftIO . M.connect $
                ci & M.setCreds (M.Credentials accessKey secretKey)
                   & M.setRegion "us-east-1"


    flip runReaderT conn . M.unMinio $ do
      bExist <- M.bucketExists "larluo"
      when (not bExist) $ void $ M.makeBucket "larluo" Nothing
      {--
      sourceTBMChan chan
              .| C.concat
              .| C.take 3
              .| C.map (cs . J.encode)
              .| C.iterM (liftIO . print)
              & \s -> M.putObject bucket "postgresql.txt" s Nothing M.defaultPutObjectOptions
      --}
      M.putObject bucket "postgresql.txt" (C.yieldMany ["aaa", "bbb"]) Nothing M.defaultPutObjectOptions
    liftIO $ putStrLn "finished"
  where
    mkTbIntefaceRow id name description xtype
                    state timeliness params result_plugin_type
                    vendor_id server_id success_code =
      ( #id := id, #name := name, #description := description, #type := xtype
      , #state := state, #timeliness := timeliness, #params := params, #result_plugin_type := result_plugin_type
      , #vendor_id := vendor_id, #server_id := server_id, #success_code := success_code)
    mkRow = mkTbIntefaceRow
                      <$> column text <*> column text <*> column text <*> column text
                      <*> column text <*> column text <*> column text <*> column text
                      <*> column text <*> column text <*> column text

