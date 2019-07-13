{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Aeson (Value(Object, Number, String, Array))
import Network.HTTP.Conduit (CookieJar)
import Data.HashMap.Lazy (HashMap)

data QRcode = QRcode {
      getQRCodeVersion :: Int,
      getQRCodeWidth   :: Int,
      getQRCodeString  :: ByteString
    } deriving (Show, Read)

data HttpST = HttpST {
  getCJ :: CookieJar
, getParserST :: Value
} deriving (Show)

data ContactFlag = Friend | Group | PublicNo
encodeContactFlag :: ContactFlag -> Int
encodeContactFlag Friend = 1
encodeContactFlag Group = 2
encodeContactFlag PublicNo = 3

data Sex = Unkown | Male | Female
encodeSex :: Sex -> Int
encodeSex Unkown = 0
encodeSex Male = 1
encodeSex Female = 2

data Contact = Contact {
  getUserName :: Text
, getNickName :: Text
, getRemarkName :: Text
, getSex :: Int
, getSignature :: Text
} deriving (Show)

data HttpContacts = HttpContacts {
  getCount:: Int
, getContacts :: [Contact]
} deriving (Show)

data SyncKey = SyncKey Int [HashMap Text Int]
  deriving (Show)

data HttpWxInitST = HttpWxInitST {
  getMe :: Contact    
, getSyncKey :: SyncKey
} deriving (Show)

data WxContext = WxContext {
  getHttpST :: HttpST
, getWxInitST :: HttpWxInitST
, getWxContacts :: [Contact]
} deriving (Show)

data Msg = Msg {
  getFromUserName :: Text
, toUserName :: Text
, getMsgType :: Int
, getContent :: Maybe Text
, getUrl :: Maybe Text
, getFileName :: Maybe Text
, getFileSize :: Maybe String
, getRecommandInfo :: Maybe Value
} deriving (Show)

data HttpMsgs = HttpMsgs {
  getAddMsgsList :: [Msg]
, getMsgsSyncKey :: SyncKey
, getMsgSyncCheckKey :: SyncKey
} deriving (Show)

data MsgType = MText | MImg | MFile | MAudio | MEmotion | MLink | MEnter | MSys | MRevert
encodeMsgType :: MsgType -> Int
encodeMsgType MText = 1
encodeMsgType MImg = 3
encodeMsgType MFile = 6
encodeMsgType MAudio = 34
encodeMsgType MEmotion = 47
encodeMsgType MLink = 49
encodeMsgType MEnter = 51
encodeMsgType MSys = 10000
encodeMsgType MRevert = 10002
{--

--}
