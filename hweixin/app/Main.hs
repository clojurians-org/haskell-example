{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Lib
import CQREncode (CQRcode(CQRcode, qr_code_version, qr_code_width, qr_code_data)
                 ,qr_apiVersionString, qr_encodeString, qr_free
                 ,qr_version_auto, qr_v01
                 ,qr_eclevel_l, qr_eclevel_m
                 ,qr_mode_num, qr_mode_an, qr_mode_eight
                 ,qr_case_on, qr_case_off )
import Foreign ((.&.))
import Foreign.C.Types (CInt(..))
import Foreign.C.String (peekCString)
import Foreign.C.Error (throwErrnoIfNull)
import Foreign.Storable (peek)

import GHC.Generics (Generic)
import Text.Printf (printf)
import System.IO (hGetEncoding, hSetEncoding, stdout, stdin, utf8
                 ,readFile, writeFile,withFile
                 ,IOMode(WriteMode))
import Data.Word (Word8)
import Data.Maybe (listToMaybe, fromJust)
import Data.Either (isRight, fromRight)
import Data.String (fromString)
import Data.List (isPrefixOf, find)
import Data.ByteString (useAsCString, packCStringLen)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC

import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Control.Applicative (empty, liftA2)
import Control.Monad((>=>), forM, join, when)
import Control.Monad.IO.Class (liftIO)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M

import System.Random (randomIO, randomRIO)
import Data.Aeson (Value(Object, String, Number), (.:), FromJSON, ToJSON)
import qualified Data.Aeson as J
import qualified Data.Attoparsec.ByteString.Lazy as P
import qualified Data.Attoparsec.Combinator as P
import Network.HTTP.Types (hContentType)
import Network.HTTP.Simple (Request, httpLBS, httpJSON, httpSink, setRequestBodyJSON)
import Network.HTTP.Conduit (CookieJar
                            , Request(cookieJar, requestHeaders, requestBody)
                            , RequestBody(RequestBodyLBS), Cookie(cookie_name, cookie_value)
                            , responseCookieJar, responseBody, destroyCookieJar
                            , method, redirectCount)

data QRcode = QRcode {
      getQRCodeVersion :: Int,
      getQRCodeWidth   :: Int,
      getQRCodeString  :: ByteString
    } deriving (Show, Read)

applyOffset :: Int -> [Word8] -> [Word8]
applyOffset n line = offset ++ line ++ offset
  where offset = replicate n 0

applyBorder :: Int -> Int -> [[Word8]] -> [[Word8]]
applyBorder n w matrix = addition ++ map (applyOffset n) matrix ++ addition
  where addition = replicate n $ replicate (2*n + w) 0

marginQR :: Int -> QRcode-> [[Word8]]
marginQR margin (QRcode _ width s) =
  applyBorder margin width $ regroup . map tobin . B.unpack $ s
  where
    tobin c = c .&. 1
    regroup [] = []
    regroup xs = let ~(this, rest) = splitAt width xs in this : regroup rest

encodeQR :: ByteString -> IO QRcode
encodeQR bs = do
  qrCodePtr <- throwErrnoIfNull "haskell-qrencode/QRcode_encodeString" $
    useAsCString (B.toStrict bs) $ \s ->
      qr_encodeString s qr_version_auto qr_eclevel_l qr_mode_eight qr_case_on
  qrCode <- peek qrCodePtr
  let version = fromIntegral . qr_code_version $ qrCode
  let width   = fromIntegral . qr_code_width $ qrCode
  str <- packCStringLen (qr_code_data qrCode , width * width)
  qr_free qrCodePtr
  return $ QRcode version width (B.fromStrict str)

toBlockLines :: [[Word8]] -> [String]
toBlockLines [] = []
toBlockLines (x1:x2:xs) = zipWith toBlock x1 x2 : toBlockLines xs
toBlockLines [x] = [map (`toBlock` 1) x]

toBlock :: Word8 -> Word8 -> Char
toBlock a b = case (a, b) of
  (0,0) -> '\x2588'
  (0,1) -> '\x2580'
  (1,0) -> '\x2584'
  _ -> ' '

catEithers :: [Either b a] -> [a]
catEithers ls = [x | Right x <- ls]

surround :: ByteString -> ByteString -> ByteString -> Either String ByteString
surround start end bs = do
  let uuidP = (P.manyTill P.anyWord8 . P.string . B.toStrict) start *>
                (P.manyTill P.anyWord8 . P.string . B.toStrict) end
  fmap B.pack . P.eitherResult $ P.parse uuidP bs

surroundTag :: ByteString -> ByteString -> Either String ByteString
surroundTag name = surround (B.concat ["<", name, ">"]) (B.concat ["<", name, "/>"])

urlBind0 :: ByteString -> Request
urlBind0 = fromString . BC.unpack
urlBind1 :: String -> ByteString -> Request
urlBind1 url = fromString . printf url . BC.unpack 

httpParse :: Request -> (ByteString -> a) -> IO a
httpParse url parse = fmap (parse . responseBody) $ httpLBS url

httpSend :: Request -> Value -> IO ByteString
httpSend url bs = fmap responseBody . httpLBS $
            url {method = "POST"
                ,requestHeaders = (hContentType, "application/json; charset=utf-8") :
                                  filter (\(y, _) -> y /= hContentType) (requestHeaders url)
                ,requestBody = RequestBodyLBS (J.encode bs)}
  
httpUUID :: IO (Either String ByteString)
httpUUID = httpParse "https://login.weixin.qq.com/jslogin?appid=wx782c26e4c19acffb" $
                     surround "window.QRLogin.uuid = \"" "\";"

printUUID :: ByteString -> IO ()
printUUID uuid = do
  -- https://login.weixin.qq.com/qrcode/{uuid}
  let url = "https://login.weixin.qq.com/l/%s"
  qrCode <- encodeQR . BC.pack . printf url . BC.unpack $ uuid
  mapM_ putStrLn . toBlockLines . marginQR 2 $ qrCode

httpQueryUUID :: ByteString -> IO ByteString
httpQueryUUID uuid = httpParse (urlBind1 "https://login.wx2.qq.com/cgi-bin/mmwebwx-bin/login?uuid=%s" uuid) id
    
waitScan :: ByteString -> IO (Either String ByteString)
waitScan uuid = go
  where go = do
          uuidResp <- httpQueryUUID uuid
          case surround "window.code=" ";" uuidResp of
            Left err ->
              putStrLn "[SYS.NOTICE] ERROR_PARSE_CODE" >> return (Left "ERROR_PARSE_CODE")
            Right "200"  ->
              putStrLn "[SYS.NOTICE] USER CONFIRM LOGIN" >>
              return (surround "window.redirect_uri=\"" "\";" uuidResp)
            Right "201" -> 
              putStrLn "[SYS.NOTICE] USER SCAN QRCODE" >> go
            Right "408" -> 
              putStrLn "[SYS.NOTICE] QRCODE EXPIRE" >> return (Left (show uuidResp))
            Right "400"  ->
              putStrLn "[SYS.NOTICE] QRCODE EXPIRE" >> return (Left (show uuidResp))
            Right s -> putStrLn ("[SYS.NOTICE] UNKNOWN CODE:" ++ (show s)) >> return (Left (show s))

data HttpST = HttpST {
  getCJ :: CookieJar
, getParserST :: Value
} deriving (Show)

-- https://wx2.qq.com/cgi-bin/mmwebwx-bin/webwxnewloginpage?ticket=A6z6wjrMhNWcYlF1mhj8Sp1O@qrticket_0&uuid=Aasbf6ATKQ==&lang=en_US&scan=1544503287";
httpCj :: ByteString -> IO HttpST
httpCj url = do
  putStrLn ("[SYS.INFO] REDIRECT URL:" ++ (show url))
  resp <- httpLBS (urlBind0 url) {redirectCount=0}
  putStrLn $ (show . responseBody $ resp)
  let cj = responseCookieJar resp
  let body = responseBody resp
  let toValue = String . T.decodeUtf8 . B.toStrict
  let parseST = Object . M.fromList . catEithers $
                  [surroundTag "wxuin" body >>= Right . (,) "Uid" . toValue
                  ,surroundTag "wxsid" body >>= Right .  (,) "Sid" . toValue
                  ,surroundTag "skey" body >>= Right . (,) "Skey" . toValue
                  ,surroundTag "pass_ticket" body >>= Right . (,) "pass_ticket" . toValue]
  return $ HttpST cj parseST
  
initCj :: IO (Either String HttpST)
initCj = do
  uuidE <- httpUUID
  case uuidE of
    Left err -> putStrLn ("[SYS.ERROR] UUID ERROR: " ++ err) >> return (Left err)
    Right uuid ->  do
      putStrLn $ "[SYS.INFO] UUID: " ++ (show uuid)
      printUUID uuid
      redirectUrlE <- waitScan uuid
      case redirectUrlE of
        Left err -> putStrLn ("[SYS.ERROR] SCAN ERROR: " ++ err) >> return (Left err)
        Right url -> Right <$> httpCj url

data Contact = Contact {
  getUserName :: Text
, getNickName :: Text
, getSex :: Int
, getSignature :: Text
} deriving (Show)

data HttpContacts = HttpContacts {
  getCount:: Int
, getContacts :: [Contact]
} deriving (Show)

data HttpMyInfo = HttpMyInfo {
  getContact :: Contact  
} deriving (Show)

instance FromJSON Contact where
  parseJSON (Object v) = do
    userName <- v .: "UserName"
    nickName <- v .: "NickName"
    sex <- v .: "Sex"
    signature <- v .: "Signature"
    return $ Contact userName nickName sex signature
  parseJSON _ = empty
  
instance FromJSON HttpContacts where
  parseJSON (Object v) = do
    count <- v .: "MemberCount"
    contacts <- v .: "MemberList"
    return $ HttpContacts count contacts
  parseJSON _ = empty

instance FromJSON HttpMyInfo where
  parseJSON (Object v) = do
    user <- v .: "User"
    return $ HttpMyInfo user

httpContacts :: HttpST -> IO [Contact]
httpContacts (HttpST cj parserST)  = httpParse
    (urlBind0 "https://wx2.qq.com/cgi-bin/mmwebwx-bin/webwxgetcontact")
      {cookieJar = Just cj}
    (maybe [] getContacts . J.decode)

-- https://wx2.qq.com/cgi-bin/mmwebwx-bin/webwxsendmsg?lang=en_US&pass_ticket=F0i5yww%2BUXGz9X%2F3F2GrJNYY8RwJ3BxPZPKQIqV55huKwbOEwfXUEOnkUt9ig1a6

-- {"Scene":0, "Msg": {"ClientMsgId":"15446094294920531", "Type":1, "ToUserName":"filehelper", "Content":"this is a very wonderful world"}}

mkTextMsg :: Text -> Text -> Text -> IO Value
mkTextMsg me to content = do
  ts <- floor . (* 1000) <$> getPOSIXTime :: IO Int
  r4 <- randomRIO (0, 9999) :: IO Int
  let msgId = fromString $ printf "%d%4d" ts r4
  return $ Object (M.fromList [("ClientMsgId", String msgId)
                              ,("Type", Number 1)
                              ,("FromUserName", String me)
                              ,("ToUserName", String to)
                              ,("Content", String content)
                              ,("LocalID", String msgId)])

httpSendMyInit :: HttpST -> IO (Maybe Contact)
httpSendMyInit (HttpST cj parserST) = httpSend
  "https://wx2.qq.com/cgi-bin/mmwebwx-bin/webwxinit" {cookieJar = Just cj}
  (Object (M.fromList [("BaseRequest", parserST)])) >>= return . fmap getContact . J.decode 

findByNickName :: Text -> [Contact] -> Maybe Contact
findByNickName s contacts = listToMaybe . filter ((== s) . getNickName) $ contacts

httpSendMsg :: WxContext -> (Text, Text) -> IO ByteString
httpSendMsg (WxContext (HttpST cj parserST) myInfo contacts) (to, message) = do
  let nameTo = getUserName $ fromJust (findByNickName to contacts)
  mkTextMsg (getUserName myInfo) nameTo message >>= \msg -> 
    httpSend "https://wx2.qq.com/cgi-bin/mmwebwx-bin/webwxsendmsg" {cookieJar = Just cj}
      (Object (M.fromList [("BaseRequest", parserST), ("Msg", msg)]))

httpSendFile :: Text -> Text -> IO ()
httpSendFile = undefined

fromCookies :: CookieJar -> HashMap Text Text
fromCookies = M.fromList .
              fmap (liftA2 (,) (T.decodeUtf8 . cookie_name) (T.decodeUtf8 . cookie_value)) .
              destroyCookieJar
-- https://wx2.qq.com/cgi-bin/mmwebwx-bin/webwxlogout?redirect=1&type=0&skey=@crypt_77ad5b54_f596b239d28dbcf50258bcaa5b442923

data WxContext = WxContext {
  getWxHttpST :: HttpST
  , getWxMyInfo :: Contact
  , getWxContacts :: [Contact]
} deriving (Show)

mkContext :: IO WxContext
mkContext = do
  httpST <- fmap (fromRight undefined) initCj
  myInfo <- fmap fromJust (httpSendMyInit httpST)  
  contacts <- httpContacts httpST
  return $ WxContext httpST myInfo contacts
  
repl :: IO ()
repl = do
  ctx <- mkContext
  let t = httpSendMsg ctx

  ts <- floor . (* 1000) <$> getPOSIXTime :: IO Int
  
  t ("文件传输助手", fromString ("HASKELL >>= 当前时间截:" ++  show ts) ) >>= print

main :: IO ()
main = undefined
{--

--}
