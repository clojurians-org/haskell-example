{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Fn where

import Prelude
import GHC.Word (Word16)
-- import Data.Either.Combinators (fromRight')
import Data.Either.Combinators (fromRight, isLeft, fromRight')
import Control.Monad (when, forM_, mapM_, void)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Control.Applicative ((<|>))

import qualified Data.ByteString as B
import Data.String.Conversions (cs)
import Data.Conduit (ConduitT, (.|), runConduit, runConduitRes, yield)
import qualified Data.Conduit.Combinators as C
import Text.Heredoc (str)

import qualified Data.Aeson as J (decode, eitherDecode, encode)
import Labels ((:=)(..))
import Labels.JSON ()

import qualified Hasql.Connection as H (Connection, Settings, settings, acquire, settings)
import qualified Hasql.Session as H (Session(..), QueryError, run, statement)
import qualified Hasql.Statement as HS (Statement(..))
import qualified Hasql.Encoders as HE (Params(..), unit)
-- import Hasql.Decoders (Row(..), singleRow, rowList, column, nullableColumn, int8, text)
import qualified Hasql.Decoders as HD (Result(..), Row(..), unit, rowList, column, text)

import Control.Monad.Trans.Resource (ResourceT, allocate, release, runResourceT)
import Control.Concurrent.STM.TBMChan (TBMChan, newTBMChanIO, closeTBMChan, writeTBMChan)
import Data.Conduit.TMChan (sourceTBMChan, sinkTBMChan)

import UnliftIO.STM (atomically)
import UnliftIO.Async (async)

import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Network.Minio as M

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



query :: IO [B.ByteString]
query = do
  let
    dsoChan = do
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
                          
      Right connection <- liftIO $ H.acquire pgSettings
      pgToChan connection sql curName cursorSize chanSize mkRow
    dseSink = do
      bExist <- M.bucketExists "larluo"
      when (not bExist) $ void $ M.makeBucket "larluo" Nothing
  runConduitRes $ do
      (lift dsoChan >>= sourceTBMChan)
              .| C.concat
              .| C.take 2
--              .| C.map ( -- ICU.fromUnicode gbkConv . ICU.toUnicode gbkConv .
--                         cs . J.encode)
--              .| C.iterM (liftIO . print)
--              .| C.map (flip B.append "\n"))
              .| C.map (cs . show) .| C.sinkList

repl = do
  recs <- query
  forM_ recs print
