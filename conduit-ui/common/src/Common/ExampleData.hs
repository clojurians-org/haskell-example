{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}

module Common.ExampleData where

import Common.Types
import Common.Class
import Prelude

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Heredoc (str)
import qualified Data.Tree as TR
import Data.Default (def)
import Data.String.Conversions (cs)

import Data.Function ((&))
import Control.Applicative (liftA2)
import Labels ((:=)(..))
import Data.Maybe (fromJust)
import qualified Data.HashMap.Lazy as M

exampleFaasCenter :: FaaSCenter
exampleFaasCenter =
  ( #dataNetwork := ( #eventPulses :=
                        (M.fromList $ fmap (liftA2 (,) epName id) exampleEventPulses)
                    , #dataCircuits :=
                        (M.fromList $ fmap (liftA2 (,) (fromJust . dciXid) id) exampleDataCircuits)
                    , #dataConduits := M.empty
--                        fmap (liftA2 (,) (fromJust . dcoXid) id) exampleDataConduits
                    , #logicFragments := M.empty
                    , #primLogicFragments := M.empty)
  , #dataSandbox := ( #stateContainers := M.empty
                    , #dataSources := M.empty
                    , #dataServices := M.empty )
  , #eventLake := ( #cronTimers := M.empty
                  , #fileWatchers := M.empty
                  , #sqlScanners := M.empty )
    )
exampleDataSources :: [DataSource]
exampleDataSources = do
  [ DSO_SQLCursor (def { dsoSQLCursorName = "tb_interface"
                       , dsoSQLCursorType = "PostgreSQL"
                       , dsoSQLCursorHost = "10.132.37.200:5432"
                       , dsoSQLCursorDatabase = "monitor"
                       , dsoSQLCursorUsername = "monitor"
                       , dsoSQLCursorPassword = "monitor"
                       , dsoSQLCursorTable = "tb_interface"
                       , dsoSQLCursorFields = []
                       , dsoSQLXid = Just 1})
    ]
exampleEventPulses :: [EventPulse]
exampleEventPulses = do
  [ def { epName = "EP_DWJobNotify"
        , epDesc = "数仓作业通知"
        , epDataCircuitValues = exampleDataCircuitValues }
    ]
exampleDataCircuitValues :: [DataCircuitValue]
exampleDataCircuitValues = do
  [ def { dcivName = "DCV_HRFilePush"
        , dcivDesc = "华瑞银行数据下传平台"
        , dcivLinkedDataCircuit = (3, "文件下传平台")}
    ]

exampleDataCircuits :: [DataCircuit]
exampleDataCircuits =
  [ def { dciName = "DataCircuitREPL"
        , dciDesc = "数据电路REPL"
        , dciXid = Just 0}
  , def { dciName = "FileLoadPlatform"
        , dciDesc = "文件加载平台"
        , dciXid = Just 1 }
  , def { dciName = "DWSchedulePlatform"
        , dciDesc = "数仓调度平台"
        , dciXid = Just 2 }
  , def { dciName = "FilePushPlatform"
        , dciDesc = "文件下传平台"
        , dciDataSandboxHolder = def
            { dsahDataSources = [ DSOH_SQLCursor ]
            , dsahDataServices = [ DSEH_FileService_MinIO ] }
        , dciPartCombinator = examplePartCombinator
        , dciXid = Just 3 }
  , def { dciName = "dataQueryPlatform"
        , dciDesc = "数据查询平台"
        , dciDataSandboxHolder = def
            { dsahDataSources = [ DSOH_SQLCursor ] }
        , dciXid = Just 4}
  , def { dciName = "ExternalDataPlatform"
        , dciDesc = "外部数据平台"
        , dciDataSandboxHolder = def
            { dsahStateContainers = [ SCH_PostgreSQL ]
            , dsahDataSources = [ DSOH_SQLCursor ] }
        , dciXid = Just 5 }
  , def { dciName = "LogPullPlatform"
        , dciDesc = "日志抽取平台"
        , dciXid = Just 6 }
  , def { dciName = "StreamingPlatform"
        , dciDesc = "流式计算平台"
        , dciXid = Just 7 }
  , def { dciName = "RealtimeAlertPlatform"
        , dciDesc = "实时预警平台"
        , dciXid = Just 8 }
  , def { dciName = "MachineLearningPlatform"
        , dciDesc = "机器学习平台"
        , dciXid = Just 9 }
    ]
  
examplePartCombinator :: TR.Tree DataCircuitPart
examplePartCombinator = do
  TR.Node def
    [ flip TR.Node [] $ DCIP_EmbededDataConduit $ def
        { dcoName = "aaa"
        , dcoPartCombinator =
            TR.Node def
              [ flip TR.Node [] $ DCOP_EmbededPrimLogicFragment $ def
                  { plfName = "conduit"
                  , plfDesc = ""
                  , plfEffectEngineCode = (Nothing, exampleConduitCode)
                    }
              ]
          }
    ]

exampleConduitCode :: T.Text
exampleConduitCode = (cs . unlines)
  [   "  void . runConduitRes . flip runReaderT dataSandbox $ do"
    , "    dataSandbox <- ask"
    , "    lift $ (L.get #dataSource dataSandbox )"
    , "      .| C.concat"
    , "      .| C.take 3"
    , "      .| C.map ((<> \"\\n\") .cs . J.encode)"
    , "      .| (L.get #dataService dataSandbox)"
    , "      .| C.sinkList"
    ]

examplePGChanCode :: T.Text
examplePGChanCode = (cs.unlines)
  [    "  let"
     , "    sql = [str|select "
     , "                |  id, name, description, 'type'"
     , "                |, state, timeliness, params, result_plugin_type"
     , "                |, vendor_id, server_id, success_code"
     , "                |from tb_interface"
     , "                |] :: B.ByteString"
     , "    "
     , "    pgSettings = H.settings \"10.132.37.200\" 5432 \"monitor\" \"monitor\" \"monitor\""
     , "    (curName, cursorSize, chanSize) = (\"larluo\", 200, 1000)"
     , "    "
     , "    textColumn = HD.column HD.text"
     , "    mkRow = (,,,,,,,,,,)"
     , "                <$> fmap (#id :=) textColumn"
     , "                <*> fmap (#name :=) textColumn"
     , "                <*> fmap (#description :=) textColumn"
     , "                <*> fmap (#type :=) textColumn"
     , "                <*> fmap (#state :=) textColumn"
     , "                <*> fmap (#timeliness :=) textColumn"
     , "                <*> fmap (#params :=) textColumn"
     , "                <*> fmap (#result_plugin_type :=) textColumn"
     , "                <*> fmap (#vendor_id :=) textColumn"
     , "                <*> fmap (#server_id :=) textColumn"
     , "                <*> fmap (#success_code :=) textColumn"
     , "                      "
     , "  chan <- do"
     , "    Right connection <- liftIO $ H.acquire pgSettings"
     , "    lift $ pgToChan connection sql curName cursorSize chanSize mkRow"
     , "  sourceTBMChan chan"
    ]
exampleMinIOCode :: T.Text
exampleMinIOCode = (cs . unlines)
  [  "  let"
   , "    (ci', accessKey, secretKey, bucket, filepath)"
   , "      = ( \"http://10.132.37.200:9000\""
   , "        , \"XF90I4NV5E1ZC2ROPVVR\""
   , "        , \"6IxTLFeA2g+pPuXu2J8BMvEUgywN6kr5Uckdf1O4\""
   , "        , \"larluo\""
   , "        , \"postgresql.txt\")"
   , "    ci = ci' & M.setCreds (M.Credentials accessKey secretKey)"
   , "             & M.setRegion \"us-east-1\""
   , "  "
   , "  Right uid <- liftIO . M.runMinio ci $ do"
   , "    bExist <- M.bucketExists bucket"
   , "    when (not bExist) $ void $ M.makeBucket bucket Nothing"
   , "    let a = M.putObjectPart"
   , "    M.newMultipartUpload bucket filepath  []"
   , "  "
   , "  C.chunksOfE (2000 * 1000)"
   , "    .| mergeSource (C.yieldMany [1..])"
   , "    .| C.mapM  (\\(pn, v) -> liftIO . M.runMinio ci $ M.putObjectPart bucket filepath uid pn [] (M.PayloadBS v))"
   , "    .| (C.sinkList >>= yield . fromRight' . sequence)"
   , "    .| C.mapM (liftIO . M.runMinio ci . M.completeMultipartUpload bucket filepath uid)"
    ]
  
tshow :: (Show a) => a -> T.Text
tshow = cs . show

repl :: IO ()
repl = do
  exampleEventPulses
    & head
    & toHaskellCodeBuilder exampleFaasCenter
    & toHaskellCode & T.putStrLn

