{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Frontend.Page.DataNetwork.DataCircuit
  (dataNetwork_dataCircuit) where

import Common.Types
import Common.ExampleData
import Common.WebSocketMessage
import Frontend.FrontendStateT
import Frontend.Widget
import Frontend.Class
import Prelude

import Reflex.Dom.Core
import Frontend.Class (ToUI(toLabel, toIcon, toDOM))

import Text.Heredoc (str)
import GHC.Generics (Generic)
import Control.Monad (forM, forM_, void)
import Control.Monad.Fix (MonadFix)

import qualified Data.Text as T
import qualified Data.Map as M
import Data.String.Conversions (cs)

import Data.Functor ((<&>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent (MVar, newMVar, putMVar, modifyMVar, modifyMVar_, readMVar, threadDelay)
import qualified Data.Tree as TR
import Labels
import Control.Lens hiding (lens)

dataCircuitDOM :: DomBuilder t m
  => TR.Tree DataCircuitPart -> m ()
dataCircuitDOM rootNode =
  divClass "ui item" $ do
    elClass "i" "random icon" blank
    divClass "content" $ do
      divClass "header" $ text "DataCircuit-数据电路"
    toDOM undefined rootNode
  

trDataCircuit :: forall t m. (DomBuilder t m, PostBuild t m)
  => Dynamic t DataCircuit -> m ()
trDataCircuit dciD = do
  tdDynInput (dciD <&> dciName)
  tdDynInput (dciD <&> dciDesc)
  tdDynInput (dciD <&> cs . show . dsahStateContainers . dciDataSandboxHolder)
  tdDynInput (dciD <&> cs . show . dsahDataSources . dciDataSandboxHolder)
  tdDynInput (dciD <&> cs . show . dsahDataServices . dciDataSandboxHolder)
  return ()

dataNetwork_dataCircuit
  :: forall t m .
     ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m
     , HasFrontendState t (FaaSCenter, WSResponseMessage) m)
  => m ()
dataNetwork_dataCircuit = do
  (stD, msgD) :: (Dynamic t FaaSCenter, Dynamic t WSResponseMessage) <- splitDynPure <$> askFrontendState
  let dcisD = stD <&> (^.. lens #dataNetwork . lens #dataCircuits . each)  
  divClass "ui segment basic" $ do
    pageHeader "数据电路" [ "由数据导管连接而成"
                          , "可包含子数据电路"
                          , "按需构建业务系统"
                          , "分布式调度"]
  
  divClass "ui segment basic" $ do
    elClass "table" "ui blue selectable table" $ do
      theadList  ["名称", "描述", "状态容器", "数据源", "数据服务"]
      simpleList dcisD $ \v ->
        trE $ selectE >> trDataCircuit  v
    divClass "" $ do
      divClass "ui top attached warning segment" $ do
        divClass "ui horizontal divided list" $ do
          divClass "item" $ do
            elClass "i" "angle double down icon" blank
            divClass "content" $ divClass "header" $ text "成功继续下一步"
          divClass "item" $ do
            elClass "i" "arrows alternate vertical icon" blank
            divClass "content" $ divClass "header" $ text "失败尝试下一步"       
          divClass "item" $ do
            elClass "i" "code icon" blank
            divClass "content" $ divClass "header" $ text "独立并行逻辑"
      divClass "ui attached segment" $ divClass "ui grid" $ divClass "eight wide column" $ do
        divClass "ui list" $ do
          divClass "ui item" $ do
            elClass "i" "tags icon" blank
            divClass "content" $ do
              divClass "header" $ text "MonadState-状态容器"
              divClass "list" $ do
                divClass "item" $ do
                  elClass "i" "code icon" blank
                  divClass "content" $ divClass "header" $ text "SQLCursor"
                
          divClass "ui item" $ do
            elClass "i" "database icon" blank
            divClass "content" $ do
              divClass "header" $ text "MonadReader-数据源"
              divClass "list" $ do
                divClass "item" $ do
                  elClass "i" "code icon" blank
                  divClass "content" $ divClass "header" $ text "RestAPI"
      
          divClass "ui item" $ do
            elClass "i" "wifi icon" blank
            divClass "content" $ do
              divClass "header" $ text "MonadWriter-数据服务"
              divClass "list" $ do
                divClass "item" $ do
                  elClass "i" "code icon" blank
                  divClass "content" $ divClass "header" $ text "NotifyService_WebHook"
          divClass "ui item" $ do
            elClass "i" "paperclip icon" blank
            divClass "content" $ do
              divClass "header" $ text "MonadFix-组件引用"
              divClass "list" $ do
                divClass "item" $ do
                  elClass "i" "folder icon" blank
                  divClass "content" $ divClass "header" $ text "数据导管"
                divClass "item" $ do
                  elClass "i" "folder icon" blank
                  divClass "content" $ divClass "header" $ text "逻辑碎片"
          dataCircuitDOM examplePartCombinator
                      
    divClass "" $ do
      divClass "ui top attached segment" $ do
        elClass "h4" "ui header" $ text "代码浏览器"
      divClass "ui attached segment" $ divClass "ui form field" $ do
        textAreaElement $ def
          & initialAttributes .~ ("rows" =: "20")
          & textAreaElementConfig_initialValue .~ exampleConduitCode
       
  return ()
