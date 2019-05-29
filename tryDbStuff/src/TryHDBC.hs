module TryHDBC where

import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
import Data.Pool

thisMain = do
  conn <- connectMySQL defaultMySQLConnectInfo {
    mysqlHost = "127.0.0.1",
    mysqlPort = 3306,
    mysqlDatabase = "mytest",
    mysqlUser = "huangziqi",
    mysqlPassword = "yang520"
                                            }
  rows <- quickQuery' conn "select * from User" []
  forM_ rows $ \row -> putStrLn $ show row


-- 创建数据库连接池
createDBPool :: IO (Pool Connection)
createDBPool = createPool newConn delConn 1 10 5
  where
    newConn :: IO Connection
    newConn = do
      print "start creating connection"
      connectMySQL defaultMySQLConnectInfo {
       mysqlHost = "127.0.0.1",
       mysqlPort = 3306,
       mysqlDatabase = "mytest",
       mysqlUser = "huangziqi",
       mysqlPassword = "yang520"
    }
    delConn :: Connection -> IO ()
    delConn conn = do
     print "closing the connection"
     disconnect conn

searchSomething :: IO()
searchSomething = do
  pool <- createDBPool
  withResource pool $ \conn -> do
    rows <- quickQuery' conn "select * from User" []
    forM_ rows $ \row -> putStrLn $ show row

--处理错误的例子
thisRun :: IO ()
thisRun = do
  pool <- createDBPool
  withResource pool $ \conn -> do
    handleSql (\err -> do
                 print $ seErrorMsg err
                 rollback conn
                 return ())
      (run conn "insert into User(id,name,password) values(?,?,?)" [toSql (1 ::Int),toSql "tname",toSql "pwd"] >>
      return ())
