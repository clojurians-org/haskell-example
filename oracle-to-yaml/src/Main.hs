{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import GHC.Generics (Generic)
import Data.Aeson (Value)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as SB
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as T
import Control.Monad.Loops (unfoldM)
import Database.Dpi (
    ExecMode(ModeExecDefault), DataValue(..), OracleConfig, SQL
  , defaultOracle, withContext, withConnection, withStatement
  , executeStatement, getRowCount, fetch, getQueryValue, getQueryInfo
  )
import Database.Dpi.Field (
    DataField(..), FromDataFields(fromDataFields'), FromDataField(fromDataField)
  )

import Data.String.Conv (toS, toSL)
import Text.Heredoc (str)

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
  fromDataField f = fmap toS <$> (fromDataField f :: IO (Maybe SB.ByteString))
  
instance FromDataFields TbInterface where
  fromDataFields' dfs = mkTbInterfaceFromList <$> sequence (fmap fromDataField dfs)

main :: IO ()
main = do
  let dbConf = defaultOracle "KB" "KB123456" "10.132.37.241:1521/EDMP"
  r :: [TbInterface] <- quickQuery dbConf
    [str|select
        |  id, name, description, 'type'
        |, state, timeliness, params, result_plugin_type
        |, vendor_id, server_id, success_code
        |from tb_interface where rownum <= 2
        |]
  T.putStrLn (toS (show r))

-- >>> repl
