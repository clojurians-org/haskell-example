{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import GHC.Generics (Generic)
import Data.Aeson (Value)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import Control.Monad.Loops (unfoldM)
import Database.Dpi (
    ExecMode(ModeExecDefault), DataValue(..), OracleConfig, SQL
  , defaultOracle, withContext, withConnection, withStatement
  , executeStatement, getRowCount, fetch, getQueryValue, getQueryInfo
  )
import Database.Dpi.Field (
    DataField(..), FromDataFields(fromDataFields'), FromDataField(fromDataField)
  )

data TbInterface = TbInterface {
    tbInterface_id :: Maybe Text
  , tbInterface_name :: Maybe Text
  , tbInterface_description :: Maybe Text
  , tbInterface_type :: Maybe Text
  , tbInterface_state :: Maybe Text
  , tbInterface_timeliness :: Maybe Text
  , tbInterface_params :: Maybe Text
  , tbInterface_result_plugin_type :: Maybe Text
  , tbInterface_vendor_id :: Maybe Text
  , tbInterface_server_id :: Maybe Text
  , tbInterface_success_code :: Maybe Text
  } deriving (Show, Generic)

mkTbInterfaceFromList (
  tbInterface_id : tbInterface_name : tbInterface_description : tbInterface_type :
  tbInterface_state : tbInterface_timeliness : tbInterface_params : tbInterface_result_plugin_type :
  tbInterface_vendor_id : tbInterface_server_id : tbInterface_success_code : []
  ) = TbInterface {..}
  
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

instance FromDataField Text where
  fromDataField f = undefined
  
instance FromDataFields TbInterface where
  fromDataFields' dfs = mkTbInterfaceFromList <$> sequence (fmap fromDataField dfs)

-- >>> :set -XOverloadedStrings
-- >>> :reload
-- Ok, one module loaded.

-- >>> let dbConf = defaultOracle "KB" "KB123456" "10.132.37.241:1521/EDMP"
-- >>> quickQuery dbConf "select * from tb_interface where rownum <= 2" :: IO [String]
-- ["\"zhengxin_doQuery\",\"????????\",\"????????\",\"assembly\",\"T\",\"0second\",,\"FETCHER\",\"times\",\"f1c5c9a2-9a35-409a-8710-c8a56c93dad6\",\"bumblebee-acquisition-server\",\"00000\"","\"zhengxin_doQuery\",\"????????\",\"????????\",\"assembly\",\"T\",\"0second\",,\"FETCHER\",\"times\",\"f1c5c9a2-9a35-409a-8710-c8a56c93dad6\",\"bumblebee-acquisition-server\",\"00000\""]
