{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric #-}

module TryPersistent where

import Control.Monad.IO.Class  (liftIO)
import Data.Aeson
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Pool
import Database.Persist
import Database.Persist.MySQL
import Database.Persist.TH

-- 通过模版自动生成的实体
share [mkPersist sqlSettings , mkMigrate "migrateAll" ] [persistLowerCase|
User
  Id Int
  name String
  password String
  deriving Show
|]

connectionInfo = defaultConnectInfo {
  connectPort = 3306,
  connectHost = "127.0.0.1",
  connectDatabase = "mytest",
  connectUser = "huangziqi",
  connectPassword = "yang520"
}


doMigrations :: SqlPersistM ()
doMigrations =
  runMigration migrateAll


--查的例子
action1 :: IO ()
action1 = do
  pool <- runStdoutLoggingT (createMySQLPool connectionInfo 10)
  flip runSqlPersistMPool pool $ do
    doMigrations
    users <- selectList ([UserId ==. UserKey 1] :: [Filter User]) []
    liftIO $ print $ map (\u -> entityKey u) users

--创建连接池
createDBPool :: IO (Pool SqlBackend)
createDBPool = do
  pool <- runStdoutLoggingT $ createMySQLPool connectionInfo 10
  return pool

--插入的例子
action2 :: IO ()
action2 = do
  pool <- createDBPool
  flip runSqlPersistMPool pool $ do
    uid <- insertKey (UserKey 7) (User "test" "test")
    liftIO $ print uid

--查的例子2，获取到数据
action3 :: Pool SqlBackend -> IO [User]
action3 pool = do
  flip runSqlPersistMPool pool $ do
    selectList ([] :: [Filter User]) [] >>= return . map entityVal

--抽象一下
actionBone pool domain = flip runSqlPersistMPool pool domain

action3' :: Pool SqlBackend -> IO [User]
action3' pool = actionBone pool $ do
  users <- selectList ([] :: [Filter User]) []
  return $ map entityVal users


-- 更新的例子
action4 :: Pool SqlBackend -> IO ()
action4 pool = actionBone pool $ do
  update (UserKey 1) [UserName =. "test" , UserPassword =. "test"]
  liftIO $ print "test"

-- 更新替换
action5 ::Pool SqlBackend -> IO ()
action5 pool = actionBone pool $ do
  let key = UserKey 1
  meu <- selectFirst [UserId ==. key] []
  case meu of
    Just eu -> replace key $ entityVal eu
    Nothing -> liftIO $ print "not found"

-- 测试一下回滚
actionRollback :: Pool SqlBackend -> IO()
actionRollback pool = actionBone pool $ do
  insertKey (UserKey 6) $ User "test" "test"
  update (UserKey 1) [UserName =. "admin" , UserPassword =. "admin"]
  insertKey (UserKey 1) $ User "test" "test"
  liftIO $ print "test"
