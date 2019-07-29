{-# LANGUAGE NoImplicitPrelude #-}
module Common.ExampleData where

import Common.Types.DataNetwork
import Common.Types.DataSandbox
import Common.Types.EventLake
import Prelude


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
            { dsahDataSources = [ DataSourceHolder_SQLCursor ]
            , dsahDataServices = [ DataServiceHolder_FileService_MinIO ] }
        , dciXid = Just 3 }
  , def { dciName = "dataQueryPlatform"
        , dciDesc = "数据查询平台"
        , dciDataSandboxHolder = def
            { dsahDataSources = [ DataSourceHolder_SQLCursor ] }
        , dciXid = Just 4}
  , def { dciName = "ExternalDataPlatform"
        , dciDesc = "外部数据平台"
        , dciDataSandboxHolder = def
            { dataSandboxHolder_stateContainers = [ StateContainerHolder_PostgreSQL ]
            , dsahDataSources = [ DataSourceHolder_SQLCursor ] }
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
        , dciXid = Just 9 } ]

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
    [ flip TR.Node [] $ DCP_EmbededDataConduit $ def
        { dataConduit_name = ""
        , dataConduit_dataSources =
            dataCircuit & dciDataSandboxHolder
                        & dsahDataSources
        , dataConduit_partCombinator =
            TR.Node def
              [ flip TR.Node [] $ DCP_EmbededLogicFragment $ def
                  { logicFragment_name = ""
                  , logicFragment_desc = ""
                  , logicFragment_effectEngineCode =
                      ( Just EE_Conduit, exampleEffectCode ) }
              ]
          }
    ]
