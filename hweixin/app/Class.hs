module Class where

import Types
import Control.Applicative (empty)
import Data.Aeson (Value(Object, String, Number), (.:), (.:?), (.=), FromJSON, ToJSON, object)
import qualified Data.Aeson as J

instance FromJSON Contact where
  parseJSON (Object v) = do
    userName <- v .: "UserName"
    nickName <- v .: "NickName"
    remarkName <- v .: "RemarkName"
    sex <- v .: "Sex"
    signature <- v .: "Signature"
    return $ Contact userName nickName remarkName sex signature
  parseJSON _ = empty
  
instance FromJSON HttpContacts where
  parseJSON (Object v) = do
    count <- v .: "MemberCount"
    contacts <- v .: "MemberList"
    return $ HttpContacts count contacts
  parseJSON _ = empty

instance FromJSON SyncKey where
  parseJSON (Object v) = do
    count <- v .: "Count"
    list <- v .: "List"
    return $ SyncKey count list
instance ToJSON SyncKey where
  toJSON (SyncKey count list) =
    object [ "Count" .= count, "List" .= list ]
  
instance FromJSON HttpWxInitST where
  parseJSON (Object v) = do
    contact <- v .: "User"
    syncKey <- v .: "SyncKey"
    return $ HttpWxInitST contact syncKey

instance FromJSON Msg where
  parseJSON (Object v) = do
    fromUserName <- v .: "FromUserName"
    toUserName <- v .: "ToUserName"
    msgType <- v .: "MsgType"
    content <- v .:? "Content"
    url <- v .:? "Url"
    fileName <- v .:? "FileName"
    fileSize <- v .:? "FileSize"
    recommandInfo <- v .:? "RecommandInfo"
    return (Msg fromUserName toUserName msgType content url fileName fileSize recommandInfo)
    
instance FromJSON HttpMsgs where
  parseJSON (Object v) = do
    addMsgList <- v .: "AddMsgList"
    syncKey <- v .: "SyncKey"
    syncCheckKey <- v .: "SyncCheckKey"
    return (HttpMsgs addMsgList syncKey syncCheckKey)
