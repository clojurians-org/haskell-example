{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend.Page.DataSandbox.DataSource.SQLCursor
  (dataSource_sqlCursor) where

import Common.Api
import Common.Types
import Common.WebSocketMessage
import Frontend.FrontendStateT
import Frontend.Widget
import Frontend.Class
import Prelude

import Control.Monad (forM)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Reflex.Dom.Core

import Data.String.Conversions (cs)
import Data.Functor ((<&>))
import Control.Lens hiding (lens)
import Labels
import Data.Default (Default(def))

trSQLCursor :: forall t m. (DomBuilder t m, PostBuild t m)
  => Dynamic t DSOSQLCursor -> m ()
trSQLCursor sqlCursorD = do
  tdDyn (sqlCursorD <&> dsoSQLCursorName)
  tdDyn (sqlCursorD <&> dsoSQLCursorDesc)
  tdDyn (sqlCursorD <&> dsoSQLCursorType)
  tdDyn (sqlCursorD <&> dsoSQLCursorDatabase)
  tdDyn (sqlCursorD <&> dsoSQLCursorTable)  
  return ()

--dataSource_selector :: 
dataSource_sqlCursor
  :: forall t m.
     ( DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m
     , HasFrontendState t (FaaSCenter, WSResponseMessage) m
     , EventWriter t [WSRequestMessage] m)     
  => m ()
dataSource_sqlCursor = do
  (stD, msgD) :: (Dynamic t FaaSCenter, Dynamic t WSResponseMessage) <- splitDynPure <$> askFrontendState
  display msgD
  let sqlCursorsD = stD <&> (^.. lens #dataSandbox . lens #dataSources . each . _DSO_SQLCursor)
      databaseRE = fforMaybe (updated msgD) $ \case
        DSOSQLCursorDatabaseRRES (Right r) ->  Just r
        _ -> Nothing
  databaseRD <- holdDyn [] databaseRE
  divClass "ui segment basic" $ do
    pageHeader "SQLCursor数据源" [ "SQL游标数据读取"
                                 , "支持PostgreSQL/Oracle/MySQL"
                                 , "根据自增字段计算增量状态" ]
    
  (submitD, submitE) <- divClass "ui segment basic" $ do
    sqlCursorE <- elClass "table" "ui selectable table" $ do
      theadList ["名称", "描述", "类型", "主机", "数据库"]
      e0 <- (trE $ createIcon >> trSQLCursor (constDyn (def :: DSOSQLCursor))) <&> tagPromptlyDyn (return def)
      e1 <- switchDyn . fmap leftmost <$> simpleList sqlCursorsD
              (\x -> (trE $ selectE >> trSQLCursor x) <&> tagPromptlyDyn x)
      return $ leftmost [e0, e1]
      
    sqlCursorD <- holdDyn def sqlCursorE

    (submitD, submitE) <- loginFormB (dsoSQLCursorHost <$> sqlCursorD) (dsoSQLCursorUsername <$> sqlCursorD) (dsoSQLCursorPassword <$> sqlCursorD)
    tellEventSingle $ flip tagPromptlyDyn submitE $
                     DSOSQLCursorDatabaseRREQ <$> submitD
                                              <*> (dsoSQLCursorType <$> sqlCursorD)
                                              <*> (dsoSQLCursorDatabase <$> sqlCursorD)

    return (submitD, submitE)
    
  submitClickedD <- holdDyn Nothing (Just () <$ submitE)
  dyn (maybe blank (const (selectorWidget submitD databaseRD)) <$> submitClickedD)
  return ()
  where
    navWidget databaseRD = do
      elClass "table" "ui selectable table" $ do
        el "thead" $ el "tr" $ trHeadList ["模式", "名称"]        
        el "tbody" $ do

          simpleList databaseRD $ \v -> el "tr" $ do
            el "td" $ dynText (v <&> (^. lens #schema))
            el "td" $ dynText (v <&> (^. lens #table))            

    selectorWidget submitD databaseRD = 
      divClass "ui segment basic" $ do
          divClass "ui grid" $ do
            divClass "four wide column" $ do
              navWidget databaseRD
            divClass "eight wide column" $ do
                elClass "table" "ui table" $ do
                  el "thead" $ el "tr" $ do
                    elClass "th" "" $ checkbox False def
                    elClass "th" "" $ text "名称"
                    elClass "th" "" $ text "类型"
                    elClass "th" "" $ text "描述"
                  el "tbody" $  do
                    el "tr" $ do
                      el "td" $ checkbox False def
                      el "td" $ text "name"
                      el "td" $ text "text"
                      el "td" $ text "NULL"
                    el "tr" $ do
                      el "td" $ checkbox False def
                      el "td" $ text "expr"
                      el "td" $ text "text"
                      el "td" $ text "NULL"
                    el "tr" $ do
                      el "td" $ checkbox False def
                      el "td" $ text "type"
                      el "td" $ text "text"
                      el "td" $ text "NULL"
          
