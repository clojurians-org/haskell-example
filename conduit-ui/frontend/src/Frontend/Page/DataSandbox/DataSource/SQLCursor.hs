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

import Control.Applicative (liftA2)
import Control.Monad (forM, void)
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
  -- display msgD
  let sqlCursorsD = stD <&> (^.. lens #dataSandbox . lens #dataSources . each . _DSO_SQLCursor)
      databaseRE = fforMaybe (updated msgD) $ \case
        DSOSQLCursorDatabaseRRES (Right r) ->  Just r
        _ -> Nothing
      tableRE = fforMaybe (updated msgD) $ \case
        DSOSQLCursorTableRRES (Right r) ->  Just r
        _ -> Nothing
        
  divClass "ui segment basic" $ do
    pageHeader "SQLCursor数据源" [ "SQL游标数据读取"
                                 , "支持PostgreSQL/Oracle/MySQL"
                                 , "根据自增字段计算增量状态" ]
    
  (submitD, submitE, sqlCursorD) <- divClass "ui segment basic" $ do
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

    return (submitD, submitE, sqlCursorD)
    
  submitClickedD <- holdDyn Nothing (Just () <$ submitE)
  dyn (maybe blank (const (selectorWidget submitD sqlCursorD databaseRE tableRE)) <$> submitClickedD)
  return ()
  where
    navWidget submitD sqlCursorD databaseRE = do
      databaseRD <- holdDyn [] databaseRE      
      elClass "table" "ui selectable table" $ do
        el "thead" $ el "tr" $ trHeadList ["模式", "表名称"]
        el "tbody" $ do
          simpleList databaseRD $ \v -> do
            tableE <- trE $ do
              el "td" $ dynText (v <&> (^. lens #schema))
              el "td" $ dynText (v <&> (^. lens #table))
            tellEventSingle $ flip tagPromptlyDyn tableE $
              DSOSQLCursorTableRREQ <$> submitD
                                    <*> (sqlCursorD <&> dsoSQLCursorType)
                                    <*> (sqlCursorD <&> dsoSQLCursorDatabase)
                                    <*> (v <&> (liftA2 (,) (^. lens #schema) (^. lens #table)))

    contentWidget tableRE = void $ do
      tableRD <- holdDyn [] tableRE
      elClass "table" "ui selectable table" $ do
        theadList ["名称", "类型", "描述"]
        el "tbody" $ do
          simpleList tableRD $ \v -> do
            trE $ do
              selectE
              el "td" $ dynText (v <&> (^. lens #name))
              el "td" $ dynText (v <&> (^. lens #type))
              el "td" $ dynText (v <&> (^. lens #desc))              
    selectorWidget submitD sqlCursorD databaseRE tableRE = 
      divClass "ui segment basic" $ divClass "ui grid" $ do
        divClass "six wide column" $ navWidget submitD sqlCursorD databaseRE
        divClass "eight wide column" $ contentWidget tableRE
