{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Class ()
import Types (QRcode(..)
            , HttpST(..), HttpWxInitST(..), SyncKey(..), HttpContacts(..), Contact(..)
            , HttpMsgs(..), Msg(..)
            , ContactFlag(..), Sex(..)
            , WxContext(..))

import Extensions (sshGet)
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

import System.IO (hPutStrLn, hClose)
import GHC.Generics (Generic)
import Control.Exception (bracket)
import Text.Printf (printf)
import System.IO (hGetEncoding, hSetEncoding, stdout, stdin, utf8
                 ,readFile, writeFile,withFile
                 ,IOMode(WriteMode))
import Data.Word (Word8)
import Data.Maybe (listToMaybe, catMaybes, fromJust)
import Data.Either (isRight, fromRight)
import Data.String (fromString)
import Data.List (isPrefixOf, find, intercalate)
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
import Control.Arrow ((>>>))
import Control.Monad((>=>), forM, join, when)
import Control.Monad.IO.Class (liftIO)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M

import Control.Concurrent (forkIO)
import System.Random (randomIO, randomRIO)
import Data.Aeson (Value(Object, String, Number), (.:), (.=), FromJSON, ToJSON, object)
import qualified Data.Aeson as J
import qualified Data.Attoparsec.ByteString.Lazy as P
import qualified Data.Attoparsec.Combinator as P
import Network.HTTP.Types (hContentType)
import Network.HTTP.Simple (Request, httpLBS, httpJSON, httpSink, setRequestQueryString)
import Network.HTTP.Conduit (CookieJar, Cookie(cookie_name, cookie_value)
                            , Request(cookieJar, requestHeaders, requestBody)
                            , RequestBody(RequestBodyLBS)
                            , responseCookieJar, responseBody, destroyCookieJar
                            , method, redirectCount)
import Network.HTTP.Client.MultipartFormData (formDataBody, formDataBodyWithBoundary
                                             , partLBS, partFileRequestBody)

import qualified Text.XML.HXT.Core as X

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

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

catEithers :: [Either b a] -> [a]
catEithers ls = [x | Right x <- ls]

surround :: ByteString -> ByteString -> ByteString -> Either String ByteString
surround start end bs = do
  let uuidP = (P.manyTill P.anyWord8 . P.string . B.toStrict) start *>
                (P.manyTill P.anyWord8 . P.string . B.toStrict) end
  fmap B.pack . P.eitherResult $ P.parse uuidP bs

surroundTag :: ByteString -> ByteString -> Either String ByteString
surroundTag name = surround (B.concat ["<", name, ">"]) (B.concat ["</", name, ">"])

urlBind0 :: ByteString -> Request
urlBind0 = fromString . BC.unpack
urlBind1 :: String -> ByteString -> Request
urlBind1 url = fromString . printf url . BC.unpack 

httpParse :: Request -> (ByteString -> a) -> IO a
httpParse url parse = fmap (parse . responseBody) $ httpLBS url

httpSendParse :: Request -> Value -> (ByteString -> a) -> IO a
httpSendParse url bs parse =
  parse . responseBody <$> httpLBS url
             {method = "POST"
              ,requestHeaders = (hContentType, "application/json; charset=utf-8") :
                                  filter (\(y, _) -> y /= hContentType) (requestHeaders url)
              ,requestBody = RequestBodyLBS (J.encode bs)}
  
httpSend :: Request -> Value -> IO ByteString
httpSend url v = httpSendParse url v id
  
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

-- https://wx2.qq.com/cgi-bin/mmwebwx-bin/webwxnewloginpage?ticket=A6z6wjrMhNWcYlF1mhj8Sp1O@qrticket_0&uuid=Aasbf6ATKQ==&lang=en_US&scan=1544503287";
httpCj :: ByteString -> IO HttpST
httpCj url = do
  putStrLn ("[SYS.INFO] REDIRECT URL:" ++ (show url))
  resp <- httpLBS (urlBind0 url) {redirectCount=0}
  -- putStrLn $ (show . responseBody $ resp)
  let cj = responseCookieJar resp
  let body = responseBody resp
  let toValue = String . T.decodeUtf8 . B.toStrict
  let parseST = Object . M.fromList . catEithers $
                  [surroundTag "wxuin" body >>= Right . (,) "Uin" . toValue
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

httpContacts :: HttpST -> IO [Contact]
httpContacts (HttpST cj parserST)  = httpParse
    "https://wx2.qq.com/cgi-bin/mmwebwx-bin/webwxgetcontact" {cookieJar = Just cj}
    (maybe [] getContacts . J.decode)

-- https://wx2.qq.com/cgi-bin/mmwebwx-bin/webwxsendmsg?lang=en_US&pass_ticket=F0i5yww%2BUXGz9X%2F3F2GrJNYY8RwJ3BxPZPKQIqV55huKwbOEwfXUEOnkUt9ig1a6

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

httpSendWxInit :: HttpST -> IO (Either String HttpWxInitST)
httpSendWxInit (HttpST cj parserST) = httpSend
  "https://wx2.qq.com/cgi-bin/mmwebwx-bin/webwxinit" {cookieJar = Just cj}
  (Object (M.fromList [("BaseRequest", parserST)])) >>= return . J.eitherDecode

-- "https://webpush.wx2.qq.com/cgi-bin/mmwebwx-bin/synccheck?r=1544712994054&skey=@crypt_77ad5b54_19633af104a711b6fead1db786fdcd19&sid=53dC2aajbSQbpeiq&uin=1936806418&deviceid=e100233450832270&synckey=1_676913366|2_676914054|3_676914026|11_676914017|201_1544712992|1000_1544690100|1001_1544689995|2001_1544682363|2007_1544531403&_=1544712941284"

fromJObject :: Value -> Maybe (HashMap Text Value)
fromJObject (Object v)  = Just v
fromJObject _ = Nothing

fromJString :: Value -> Maybe Text
fromJString (String v) = Just v
fromJString _ = Nothing

-- retcode : 0 -> OK, 1100 -> ERROR
-- selector : 0 -> OK, 2 -> NEW_MESSAGE, 7 -> ENTER/LEAVE

encodeSyncKey :: SyncKey -> Text
encodeSyncKey (SyncKey _ list)  = do
  let mkTuple m =  M.lookup "Key" m >>= \k -> M.lookup "Val" m >>= \v -> return (k, v)
  fromString . intercalate "|" . fmap (uncurry (printf "%d_%d")) $
    fmap (fromJust . mkTuple) list

httpSyncCheck :: WxContext -> Maybe SyncKey -> IO ByteString
httpSyncCheck (WxContext (HttpST cj parserST) (HttpWxInitST me syncKey0) contacts)
              syncKey = do
  let fromParserST st = zip ["skey", "sid", "uin"] $
                            fmap (maybe Nothing (fmap T.encodeUtf8 . fromJString) .
                                  flip M.lookup (maybe M.empty id . fromJObject $ st))
                                  ["Skey", "Sid", "Uin"]
  let syncKeyText = encodeSyncKey (maybe syncKey0 id syncKey)
  -- putStrLn $ "syncKey:" ++ (show syncKeyText)
  httpParse (setRequestQueryString (("synckey", Just (T.encodeUtf8 syncKeyText)):(fromParserST parserST))
              "https://webpush.wx2.qq.com/cgi-bin/mmwebwx-bin/synccheck")
            {cookieJar = Just cj}
            id

-- "https://wx2.qq.com/cgi-bin/mmwebwx-bin/webwxsync?sid=53dC2aajbSQbpeiq&skey=@crypt_77ad5b54_19633af104a711b6fead1db786fdcd19"
httpSyncMsgs :: WxContext -> Maybe SyncKey -> IO (Either String HttpMsgs)
httpSyncMsgs (WxContext (HttpST cj parserST) (HttpWxInitST me syncKey0) contacts)
             syncKey = do
  httpSendParse "https://wx2.qq.com/cgi-bin/mmwebwx-bin/webwxsync" {cookieJar = Just cj}
    (Object (M.fromList [("BaseRequest", parserST),
                           ("SyncKey", J.toJSON (maybe syncKey0 id syncKey)) ]))
    J.eitherDecode
    
syncMsgs :: WxContext -> Maybe SyncKey -> IO (Either String HttpMsgs)
syncMsgs ctx syncKey = do
  resp <- httpSyncCheck ctx syncKey
  case (surround "{retcode:\"" "\"," resp, surround "selector:\"" "\"}" resp) of
    (Right "0", Right "0") ->
      putStrLn "[SYS.NOTICE] WAITING NEW MESSAGE..." >>
      syncMsgs ctx syncKey
    (Right "0", Right "2") ->
      putStrLn "[SYS.NOTICE] YOU HAVE NEW MESSAGES! DO SYNCING..." >>
      httpSyncMsgs ctx syncKey
    (Right "1101", _) -> putStrLn "[APP.ERROR] HTTP_REQUEST_ERROR!" >> return (Left "HTTP_REQUEST_ERROR")
    _  -> putStrLn "[SYS.ERROR] SYNC CHECK PARSE ERROR!" >>
            return (Left "SYNC_CHECK_PARSE_ERROR")

stripKeyword :: Text -> Text -> Text
stripKeyword keyword = T.strip . fromJust . T.stripPrefix keyword

doAction :: WxContext -> Text -> Text -> IO ()
doAction ctx to cmd = do
  putStrLn $ "[DBUG] doAction: " ++ (show cmd)
  case cmd of
    s | T.isPrefixOf "sshGet::File" s -> do
        (fileName, bs) <- either ((,) (Just "ERROR.txt"). fromString) id <$>
            sshGet ((B.fromStrict . T.encodeUtf8) (stripKeyword "sshGet::File" s))
        let fileName' = (maybe "result.txt" id . fmap fromString) fileName
        httpSendFileMsg' ctx (to, (fileName', bs)) >>= print
  
    s | T.isPrefixOf "sshGet" s -> do
        (fileName, bs) <- either ((,) (Just "ERROR.txt"). fromString) id <$>
            sshGet ((B.fromStrict . T.encodeUtf8) (stripKeyword "sshGet" s))
        httpSendTextMsg' ctx (to, (T.decodeUtf8 . B.toStrict)  bs) >>= print
    _ -> return ()

syncMsgsLoop :: WxContext -> IO ()
syncMsgsLoop ctx = go ctx Nothing 
  where go (WxContext (HttpST cj parserST) (HttpWxInitST me _) contacts) syncKey = do
          resp <- syncMsgs ctx syncKey
          case resp of
            Right (HttpMsgs msgs  syncKey' syncChekKey)  -> do
              forM msgs $ \(Msg from to msgType content url filename filesize recommandInfo)
                -> do
                case content of
                  Just s | T.isPrefixOf "&gt;&gt;=" s  && from == getUserName me ->
                             doAction ctx to (stripKeyword "&gt;&gt;=" s)
                  _ -> return ()
                putStrLn $ printf "[%s->%s::%d] %s"
                  (maybe "?" id (idToName from (me : contacts)))
                  (maybe "?" id (idToName to (me : contacts)))
                  msgType (maybe "<NIL>" id content)
              -- putStrLn ("[DEBUG] NEW SYNC KEY: " ++  show syncKey')
              go ctx (Just syncKey')
            Left x  -> putStrLn x

idToName :: Text -> [Contact] -> Maybe Text
idToName s contacts = fmap getNickName . listToMaybe . filter ((== s) . getUserName) $ contacts

nameToId :: Text -> [Contact] -> Maybe Text
nameToId s contacts = fmap getUserName . listToMaybe . filter ((== s) . getNickName) $ contacts

httpSendTextMsg' :: WxContext -> (Text, Text) -> IO ByteString
httpSendTextMsg' (WxContext (HttpST cj parserST) (HttpWxInitST me _) contacts)
            (to, message) = do
  mkTextMsg (getUserName me) to message >>= \msg -> 
    httpSend "https://wx2.qq.com/cgi-bin/mmwebwx-bin/webwxsendmsg" {cookieJar = Just cj}
      (Object (M.fromList [("BaseRequest", parserST), ("Msg", msg)]))

httpSendTextMsg :: WxContext -> (Text, Text) -> IO ByteString
httpSendTextMsg ctx (to, message) =
  httpSendTextMsg' ctx (fromJust (nameToId to (getWxContacts ctx)), message)

httpUploadFile :: WxContext -> (Text, (Text, ByteString)) -> IO (Either String ByteString)
httpUploadFile (WxContext (HttpST cj parserST) (HttpWxInitST me _) contacts)
               (to, (fileName, fileContent)) = do
  let url = "https://file.wx2.qq.com/cgi-bin/mmwebwx-bin/webwxuploadmedia?f=json" {cookieJar = Just cj}
  url' <- formDataBody
               [partLBS "id" "WU_FILE_1"
               ,partLBS "name" ((B.fromStrict . T.encodeUtf8) fileName)
               ,partLBS "size" ((fromString . show . B.length) fileContent)
               ,partLBS "mediatype" "doc"
               ,partLBS "uploadmediarequest"
                  ((J.encode . Object) (M.fromList [("BaseRequest", parserST)
                                                   ,("UploadType", Number 2)
                                                   ,("MediaType", Number 4)
                                                   ,("FromUserName", String (getUserName me))
                                                   ,("StartPos", Number 0)
                                                   ,("ClientMediaId", Number 1544861291684)
                                                   ,("TotalLen", Number 26825)
                                                   ,("DataLen", Number 26825)
                                                   ,("ToUserName", (String to))]))
               ,partFileRequestBody "filename" (T.unpack fileName) (RequestBodyLBS fileContent) ]
                url 
  putStrLn $ "[DEBUG]" ++ show url'
  surround "\"MediaId\": \"" "\"," . responseBody <$> httpLBS url'

mkFileMsg :: Text -> Text -> (Text, ByteString) -> ByteString -> IO Value
mkFileMsg me to (fileName, fileContent) mediaId = do
  ts <- floor . (* 1000) <$> getPOSIXTime :: IO Int
  r4 <- randomRIO (0, 9999) :: IO Int
  let msgId = fromString $ printf "%d%4d" ts r4
  content <-  X.runX (X.root [] [X.mkelem "appmsg" [X.sattr "appid" "wxeb7ec651dd0aefa9"
                                                   ,X.sattr "sdkver" ""]
                                 [X.selem "title" [X.txt (T.unpack fileName)]
                                 ,X.selem "desc" [X.txt ""]
                                 ,X.selem "action" [X.txt ""]
                                 ,X.selem "type" [X.txt "6"]
                                 ,X.selem "content" [X.txt ""]
                                 ,X.selem "url" [X.txt ""]
                                 ,X.selem "lowurl" [X.txt ""]
                                 ,X.selem "appattach"
                                  [X.selem "totallen" [X.txt ((show . B.length) fileContent)]
                                  ,X.selem "attachid" [X.txt (BC.unpack mediaId)]
                                  ,X.selem "extinfo" [X.txt ""]]]]
                      >>>  X.writeDocumentToString [X.withIndent X.no])
  return $ Object (M.fromList [("ClientMsgId", String msgId)
                              ,("Type", Number 6)
                              ,("FromUserName", String me)
                              ,("ToUserName", String to)
                              ,("Content", String ((fromString . head) content))
                              ,("LocalID", String msgId)])

httpSendFileId :: WxContext -> (Text, (Text, ByteString)) -> ByteString -> IO ByteString
httpSendFileId (WxContext (HttpST cj parserST) (HttpWxInitST me _) contacts)
                (to, (fileName, fileContent)) mediaId= do
  mkFileMsg (getUserName me) to (fileName, fileContent) mediaId >>= \msg ->
      httpSend "https://wx2.qq.com/cgi-bin/mmwebwx-bin/webwxsendappmsg?fun=async&f=json"
             {cookieJar = Just cj}
        (Object (M.fromList [("BaseRequest", parserST), ("Msg", msg)]))


httpSendFileMsg' :: WxContext -> (Text, (Text, ByteString)) -> IO ByteString
httpSendFileMsg' ctx msgInfo = do
  mediaId <- fromRight undefined <$>httpUploadFile ctx msgInfo
  httpSendFileId ctx msgInfo mediaId

httpSendFileMsg :: WxContext -> (Text, (Text, ByteString)) -> IO ByteString
httpSendFileMsg ctx (to, fInfo) = httpSendFileMsg' ctx (fromJust (nameToId to (getWxContacts ctx)), fInfo)
  
-- https://wx2.qq.com/cgi-bin/mmwebwx-bin/webwxlogout?redirect=1&type=0&skey=@crypt_77ad5b54_f596b239d28dbcf50258bcaa5b442923

nowTs :: IO Int
nowTs = floor . (* 1000) <$> getPOSIXTime

mkContext :: IO ((Text, Text) -> IO ByteString, (Text, (Text, ByteString)) -> IO ByteString)
mkContext = do
  httpST <- fmap (fromRight undefined) initCj
  wxInitST <- fmap (fromRight undefined) (httpSendWxInit httpST) 
  contacts <- httpContacts httpST
  let ctx = WxContext httpST wxInitST contacts
  forkIO (syncMsgsLoop ctx) 
  return (httpSendTextMsg ctx, httpSendFileMsg ctx)

repl :: IO ()
repl = do
  (t, f) <- mkContext
  nowTs >>= \ts -> t ("文件传输助手", fromString ("HASKELL >>= 当前时间截:" ++  show ts) ) >>= print
  t ("文件传输助手", ">>= hello") >>= print
  t ("文件传输助手", ">>= sshGet larluo:larluo@localhost:/home/larluo/") >>= print
  f ("文件传输助手", ("larluo.txt", "hello world, larluo")) >>= print
  
  
main :: IO ()
main = undefined
