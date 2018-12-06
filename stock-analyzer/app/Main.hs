{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace (trace, traceShow)
import Text.Printf (printf)
import Data.List.Split (chunksOf)
import Data.Either (isLeft, isRight)
import Data.Maybe (isJust, isNothing, catMaybes, listToMaybe, mapMaybe)
import Data.Monoid (Sum(Sum), getSum)
import Data.Foldable (find)
import Data.String (fromString)
import Data.List (transpose, tails, init)
import Control.Applicative (many, pure)
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (MonadResource)
import Data.Scientific (toRealFloat, Scientific)

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString, toStrict, fromStrict, writeFile, readFile)
import Data.Text (Text, pack, unpack, isPrefixOf, dropWhileEnd)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified Data.HashMap.Lazy as M
import Data.HashMap.Lazy (HashMap(..), fromList, fromListWith)
import qualified Data.Vector as V
import Data.Time.Clock.POSIX (getPOSIXTime, POSIXTime)
import Data.Time (UTCTime, ZonedTime
                , defaultTimeLocale, getZonedTime
                , parseTimeM, formatTime)

import Conduit (runConduit, runConduitRes, runConduitPure
              , yield, yieldMany, leftover
              , mapC, mapMC, mapM_C, takeC, iterMC, filterC, iterMC
              , (.|), ZipConduit(ZipConduit), getZipConduit
              , sinkList)
import Data.Conduit (ConduitT, await, yield, awaitForever, mergeSource)
import Data.Conduit.Combinators (chunksOfE, sinkFile, foldM)
import qualified Data.Conduit.List as CL

import qualified Data.Conduit.Combinators as C
import Data.Aeson (encode, decode, Value(Object, Array, Bool, Number, String))
import Control.Lens ((^.), (^..), (^?), (?~), (&), at)
import Data.Aeson.Lens (key, values, members, nth)

import Text.HTML.DOM (sinkDoc)
import Text.XML.Cursor
import Codec.Xlsx (Xlsx, Worksheet, CellValue(CellText, CellDouble), def
                  ,fromXlsx, cellValueAt, atSheet)

import Data.Attoparsec.ByteString.Lazy (Parser, parse
                                     , anyWord8, string, skipWhile
                                     , maybeResult, eitherResult, endOfInput)
import Data.Attoparsec.Combinator ( try, manyTill, lookAhead )

import Network.HTTP.Simple (httpSink)
import Network.HTTP.Conduit (httpLbs, Request(cookieJar), Response, Manager, CookieJar
                           , newManager, tlsManagerSettings
                           , parseRequest, responseCookieJar, responseBody)
import Web.Cookie (parseSetCookie)
import JSON (json, skipSpace)

data MatchMode = Exact !Text | Start !Text

jsonify :: ByteString -> Either String Value
jsonify snb = eitherResult . parse (json <* skipSpace <* endOfInput) $ snb

fromJObject :: Value -> Maybe (HashMap Text Value)
fromJObject (Object v)  = Just v
fromJObject _ = Nothing

fromJArray :: Value -> Maybe [Value]
fromJArray (Array v) = Just (V.toList v)
fromJArray _ = Nothing

fromJNumber :: Value -> Maybe Scientific
fromJNumber (Number v) = Just v
fromJNumber _ = Nothing

fromJString :: Value -> Maybe Text
fromJString (String v) = Just v
fromJString _ = Nothing

mkDTs :: IO [Text]
mkDTs = do
  now <- getZonedTime
  let year = formatTime defaultTimeLocale "%Y" now
      dt = formatTime defaultTimeLocale "%Y-%m-%d" now
    in
    pure . fmap fromString $
      filter (<= dt) [year ++ "-12-31", year ++ "-09-30", year ++ "-06-30", year ++ "-03-31"] ++
                     [show (read year - 1) ++ "-12-31", show (read year - 2) ++ "-12-31" ]

toLocalTS :: Text -> Maybe Integer
toLocalTS ts = do
  utc <- parseTimeM True defaultTimeLocale "%Y-%m-%d %z" (unpack ts ++ " +0800") :: Maybe UTCTime
  Just . (* 1000) . read $ (formatTime defaultTimeLocale "%s" utc)

{--
  runConduit $ yieldMany [1..8] .| before "c0" (mapC show) .| mapM_C print
  runConduit $ yieldMany [1..8] .| getZipConduit (ZipConduit (before "c0" (mapC show)) *> ZipConduit (before "c1" (mapC show)) ).| mapM_C print
--}
mkSheet :: Worksheet -> (Int, [Either String [Maybe Value]]) -> Worksheet
mkSheet xlsx (rn, vs) = do
  let setRow [] cn acc = acc
      setRow (x:xs) cn acc = case x of
        Nothing -> setRow xs (cn+1) (acc & cellValueAt (rn, cn) ?~ CellText "/")
        Just (Number n) -> setRow xs (cn+1) (acc & cellValueAt (rn, cn) ?~
                                  (CellText . fromString . printf "%.2f" $ (toRealFloat n :: Double)))
        Just (String s) -> setRow xs (cn+1) (acc & cellValueAt (rn, cn) ?~ CellText s)
        Just s -> setRow xs (cn+1) (acc & cellValueAt (rn, cn) ?~ CellText (decodeUtf8 . toStrict . encode $ s))
    in
    either (const xlsx) (\x -> setRow (concat x) 1 xlsx) (sequence vs)

writeXlsx :: String -> Worksheet -> IO ()
writeXlsx fname sheet = do
  ct <- getPOSIXTime
  let xlsx = def & atSheet "data" ?~ sheet
    in B.writeFile fname $ fromXlsx ct xlsx

selectSNB :: ByteString -> Either String ByteString
selectSNB bs = 
  B.pack <$> (eitherResult . parse snbParser $ bs)
    where snbParser = (manyTill anyWord8 (string "\nSNB = ")) *>
                      (manyTill anyWord8 (string ";"))

extractSNB :: Value -> Maybe [Maybe Value]
extractSNB v = do
  quote <- Just v >>= fromJObject >>= M.lookup "data" >>= fromJObject >>= M.lookup "quote" >>= fromJObject
  Just [ M.lookup "name" quote
       , M.lookup "current" quote
       , M.lookup "market_capital" quote >>= fromJNumber >>= Just . String . fromString . show . round . (/ 10 ^8)
       , M.lookup "pe_ttm" quote
       , M.lookup "pb" quote]

parseSNB :: Manager -> [Text] -> ConduitT Text (Either String [Maybe Value]) IO ()
parseSNB manager header = do
  let url = "https://xueqiu.com/S/%s"
      theader = fmap (Just . String) header
      parse code = do
        resp <- httpLbs (fromString (printf url (unpack code))) manager
        return $ selectSNB (responseBody resp) >>= jsonify >>= maybe (Left "ParseSNB JSON FIELD ERROR!") Right . extractSNB
    in do
      yield (Right theader) >> (mapMC parse)

extractPE :: [(Text, Integer)] -> Value -> Maybe [Maybe Value]
extractPE dtTSPair v = do
  vs <- Just v >>= fromJObject >>= M.lookup "data" >>= fromJObject >>= M.lookup "list" >>= fromJArray
  let toTuple v = do
        m <- Just v >>= fromJObject
        report_date <- M.lookup "report_date" m
        avg_roe <- M.lookup "avg_roe" m >>= fromJArray >>= listToMaybe
        Just (report_date, avg_roe)
      metrics = fromList . mapMaybe toTuple $ vs
      tss = fmap snd dtTSPair
      lookupTS ts = M.lookup (Number (fromInteger ts)) metrics
    in
      Just $ fmap lookupTS tss
  
parsePE :: CookieJar -> Manager -> [Text] -> ConduitT Text (Either String [Maybe Value]) IO ()
parsePE cj manager header = do
  dtTSPair <- liftIO $ mapMaybe (\dt -> (toLocalTS dt) >>= \ts -> Just (dt, ts)) <$> mkDTs
  let url = "https://stock.xueqiu.com/v5/stock/finance/cn/indicator.json?symbol=%s&count=20"
      dts = fmap (T.replace "-" "" . fst) dtTSPair
      theader = header >>= \name -> dts >>= \dt -> [Just . String $ T.concat [name , "-",  dt]]
      parse code = do
        resp <- httpLbs ((fromString . printf url . unpack $ code) {cookieJar = Just cj}) manager
        dtTSPair <- mapMaybe (\dt -> (toLocalTS dt) >>= \ts -> Just (dt, ts)) <$> mkDTs
        return $ jsonify (responseBody resp) >>= maybe (Left "parsePE JSON FIELD ERROR!") Right . extractPE dtTSPair
    in do
    yield (Right theader) >> (mapMC parse)

extractStock ::[(Text, MatchMode)] -> Value -> Maybe [Maybe Value]
extractStock header v = do
  items <- Just v >>= fromJObject >>= M.lookup "data" >>= fromJObject >>= M.lookup "items" >>= fromJArray
  let whichMode v (abbr, (Exact name))  | name == v = True
      whichMode v (abbr, (Start name))  | isPrefixOf name v = True
      whichMode _ _ = False
      checkStockName v = fst <$> find (whichMode v) header
      toTuple v = do
        Just v >>= fromJObject >>= \m ->
          M.lookup "holder_name" m >>= fromJString >>= checkStockName >>= \name ->
            M.lookup "held_ratio" m >>= fromJNumber >>= \value -> 
              Just (name, value)
      metrics = fromListWith (+) $ mapMaybe toTuple items
      lookupStock (name, _) = Number <$> M.lookup name metrics
    in
      Just $ fmap lookupStock header 

parseStock :: CookieJar -> Manager -> [(Text, MatchMode)] -> ConduitT Text (Either String [Maybe Value]) IO ()
parseStock cj manager header = do
  dtTSPair <- liftIO $ mapMaybe (\dt -> (toLocalTS dt) >>= \ts -> Just (dt, ts)) <$> mkDTs
  let url = "https://stock.xueqiu.com/v5/stock/f10/cn/top_holders.json?symbol=%s&circula=0&count=1&start=%d"
      dts = fmap (T.replace "-" "" . fst) dtTSPair
      theader = header >>= \(name, mode) -> dts >>= \dt -> [Just . String $ T.concat [name , "-",  dt]]
      parse code = do
        parts <- fmap sequence . forM dtTSPair $ \(dt, ts) ->  do
                resp <- httpLbs ((fromString (printf url (unpack code) ts)) {cookieJar = Just cj}) manager
                pure $ jsonify (responseBody resp) >>= maybe (Left "parsePE JSON FIELD ERROR!") Right . extractStock header
        return $ parts >>= Right . concat . transpose
    in do
      yield (Right theader) >> (mapMC parse)

parseCodes :: IO [Text]
parseCodes = do
  let url = "http://vip.stock.finance.sina.com.cn/corp/go.php/vII_NewestComponent/indexid/000016.phtml"
    in do
    doc <- httpSink url $ const sinkDoc
    let vs = (fromDocument doc) $// attributeIs "id" "NewStockTable"
                       &// element "td"
                       &/ element "div"
                       &/ content
      in
      return $ map (T.append "SH" . head) (chunksOf 2 vs)

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  homeResp <- httpLbs "https://xueqiu.com" manager
  codes <- parseCodes
  let cj = responseCookieJar homeResp
    in do 
    sheet <- runConduit $ yieldMany codes
                .| getZipConduit (ZipConduit (parseSNB manager ["名称", "价格", "市值(亿)", "市盈率", "市净率"]) *>
                                  ZipConduit (parsePE cj manager ["资产收益率"]) *>
                                  ZipConduit (parseStock cj manager
                                                        [("股东占比.中国证券", Exact "中国证券金融股份有限公司"),
                                                         ("股东占比.中央汇金", Exact "中央汇金资产管理有限责任公司"),
                                                         ("股东占比.全国社保基金", Start "全国社保基金")] ))
                .| CL.chunksOf 3
                -- .| takeC 3
                .| mergeSource (yieldMany [1..])
                .| C.foldl mkSheet def
    writeXlsx "stock-alazyer.xlsx" sheet
    putStrLn "finished"

{--
  main
--}
