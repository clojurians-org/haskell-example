{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}

module Common.ExampleData where

import Common.Types
import Prelude

import qualified Data.Text as T
import Text.Heredoc (str)
import qualified Data.Tree as TR
import Data.Default (def)

import Data.Function ((&))
import Control.Applicative (liftA2)
import Labels ((:=)(..))
import Data.Maybe (fromJust)
import qualified Data.HashMap.Lazy as M

exampleAppST :: AppST
exampleAppST =
  ( #dataNetwork := ( #eventPulses :=
                        (M.fromList $ fmap (liftA2 (,) epName id) exampleEventPulses)
                    , #dataCircuits :=
                        (M.fromList $ fmap (liftA2 (,) (fromJust . dciXid) id) exampleDataCircuits)
                    , #dataConduits := M.empty
--                        fmap (liftA2 (,) (fromJust . dcoXid) id) exampleDataConduits
                    , #logicFragments := M.empty )
  , #dataSandbox := ( #stateContainers := M.empty
                    , #dataSources := M.empty
                    , #dataServices := M.empty )
  , #eventLake := ( #cronTimers := M.empty
                  , #fileWatchers := M.empty
                  , #sqlScanners := M.empty )
    )

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
        , dcivLinkedDataCircuit = (7, "文件下传平台")}
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

exampleEffectCode :: T.Text
exampleEffectCode =
  [str|do
      |  sourceTBMChan {{ DataSource_SQLCursor }}
      |    .| C.take 2
      |    .| {{ DataService_FileService_MinIO }}
      |]

examplePartCombinator :: DataCircuit -> TR.Tree DataCircuitPart
examplePartCombinator dataCircuit = do
  TR.Node def
    [ flip TR.Node [] $ DCIP_EmbededDataConduit $ def
        { dcoName = ""
        , dcoDataSources =
            dataCircuit & dciDataSandboxHolder
                        & dsahDataSources
        , dcoPartCombinator =
            TR.Node def
              [ flip TR.Node [] $ DCOP_EmbededLogicFragment $ def
                  { lfName = ""
                  , lfDesc = ""
                  , lfEffectEngineCode =
                      ( Just EE_Conduit, exampleEffectCode ) }
              ]
          }
    ]

exampleCode :: String
exampleCode = unlines [
          "do"
        , "  let sql = [str|select"
        , "                |  id, name, description, 'type'"
        , "                |, state, timeliness, params, result_plugin_type"
        , "                |, vendor_id, server_id, success_code"
        , "                |from tb_interface"
        , "                |] :: B.ByteString"
        , "  let pgSettings = H.settings \"10.132.37.200\" 5432 \"monitor\" \"monitor\" \"monitor\""
        , "  let (curName, cursorSize, chanSize) = (\"larluo\", 200, 1000)"
        , "  let textColumn = HD.column HD.text"
        , "  let mkRow = (,,,,,,,,,,)"
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
        , "  Right connection <- liftIO $ H.acquire pgSettings"
        , "  runResourceT $ do"
        , "    chan <- pgToChan connection sql curName cursorSize chanSize mkRow"
        , "    runConduit $"
        , "      (sourceTBMChan chan"
        , "            .| C.concat"
        , "            .| C.take 2"
        , "            .| C.mapM_ (liftIO . print))"
          ]

              
