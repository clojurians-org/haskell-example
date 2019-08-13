{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Test.Hspec

import           Control.Exception     (SomeException, catch)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import           Data.Monoid           ((<>))
import           Data.Scientific
import           Data.Time
import           System.Environment

import           Database.Dpi
import           Database.Dpi.Field
import           Database.Dpi.Sql

ioConf :: IO (Maybe OracleConfig)
ioConf = do
  envs <- getEnvironment
  return $ do
    username <- lookup "DB_USER" envs
    password <- lookup "DB_PASS" envs
    url      <- lookup "DB_URL"  envs
    return $ defaultOracle (BC.pack username) (BC.pack password) (BC.pack url)

main :: IO ()
main = do
  c <- ioConf
  case c of
    Just con -> setupLanguage con >> test con
    _        -> putStrLn "Ignore test"

prepareTable :: PtrConn -> IO ()
prepareTable conn = do
  let g :: SomeException -> IO Int
      g _ = return 0
  _ <- execute conn "DROP TABLE TEST_T_NAME" [] `catch` g
  _ <- execute conn "CREATE TABLE TEST_T_NAME(ID INTEGER PRIMARY KEY, NAME VARCHAR2(64) NOT NULL, CREATE_TIME TIMESTAMP WITH TIME ZONE DEFAULT SYSTIMESTAMP NOT NULL)" []
  _ <- execute conn "DROP SEQUENCE TEST_SEQ_NAME" [] `catch` g
  _ <- execute conn "CREATE SEQUENCE TEST_SEQ_NAME" []
  return ()

data IdName = IdName
  { key  :: Int
  , name :: B.ByteString
  , time :: ZonedTime
  } deriving Show

instance FromDataFields IdName where
  fromDataFields dfm = do
    Just key  <- readDataField dfm "ID"
    Just name <- readDataField dfm "NAME"
    Just time <- readDataField dfm "CREATE_TIME"
    return IdName{..}

data S = S  { avg :: Scientific } deriving Show

instance FromDataFields S where
  fromDataFields dfm = do
    Just avg <- readDataField dfm "AVG"
    return S{..}

test :: OracleConfig -> IO ()
test conf = hspec $ do
  describe "Database.Dpi" $ do

    it "Context Test" $ do
      c       <- createContext
      version <- getClientVersion c
      ok      <- destroyContext   c
      print version
      ok `shouldBe` True

    it "Connection Test" $ withContext $ \cxt -> do
      conn  <- createConnection cxt conf return
      pOk   <- pingConnection conn
      pOk `shouldBe` True
      getEncodingInfo  conn >>= print
      getStmtCacheSize conn >>= print
      getServerVersion conn >>= print
      getLTXID         conn >>= print
      getInternalName  conn >>= print
      getExternalName  conn >>= print
      getEdition       conn >>= print
      getCurrentSchema conn >>= print
      ok    <- releaseConnection conn
      ok `shouldBe` True
      notOk <- releaseConnection conn
      notOk `shouldBe` False
      pNOk  <- pingConnection conn
      pNOk `shouldBe` False

    it "Statement 1 Test" $ withContext $ \cxt -> do
      withConnection cxt conf return $ \conn -> do
        st <- prepareStatement conn False "SELECT '中文' FROM DUAL"
        c  <- getBindCount st
        c  `shouldBe` 0
        n  <- getBindNames st
        n  `shouldBe` []
        getFetchArraySize     st >>= print
        getImplicitResult     st >>= print
        getStatementInfo      st >>= print

        r  <- executeStatement st ModeExecDefault
        r `shouldBe` 1
        qc <- getNumberQueryColumns st
        qc `shouldBe` r

        getImplicitResult     st >>= print
        info <- getQueryInfo  st 1

        mayC <- fetch st
        mayC `shouldBe` Just 0

        value <- getQueryValue st 1
        Just k :: Maybe B.ByteString <- fromDataField DataField{..}
        print info
        BC.putStrLn k

        ok <- releaseStatement st
        ok `shouldBe` True

    it "Statement 2 Test" $ withContext $ \cxt -> do
      withConnection cxt conf return $ \conn -> do
        withStatement conn False "SELECT 1,'中文' as 文字,SYSDATE FROM DUAL" $ \st -> do
          r <- executeStatement st ModeExecDefault
          r `shouldBe` 3
          f <- fetch st
          f `shouldBe` Just 0
          v <- mapM (getQueryValue st) [1..r]
          print v
        withStatement conn False "SELECT 1,'hhh',SYSDATE FROM DUAL" $ \st -> do
          r <- executeStatement st ModeExecDefault
          r `shouldBe` 3
          f <- fetch st
          f `shouldBe` Just 0
          mapM (getQueryValue st) [1..r] >>= print
          f2 <- fetch st
          f2 `shouldBe` Nothing

    it "Statement 3 Failed Test" $ withContext $ \cxt -> do
      withConnection cxt conf return $ \conn -> do
        withStatement conn False "Wrong sql" $ \st -> do
          executeStatement st ModeExecDefault `shouldThrow` anyException

    it "Pool Test" $ withContext $ \cxt -> do
      Database.Dpi.withPool cxt conf return $ \pool ->
        withPoolConnection pool $ \conn -> do
          let f = withStatement conn False "SELECT 1,'中文' as 文字,SYSTIMESTAMP FROM DUAL" $ \st -> do
                      r <- executeStatement st ModeExecDefault
                      _ <- fetch st
                      mapM (getQueryValue st) [1..r] >>= print
          _ <- sequence $ take 2 $ repeat f
          (queryByPage conn "SELECT DBTIMEZONE,sessiontimezone,CURRENT_DATE,CURRENT_TIMESTAMP,SYSDATE,SYSTIMESTAMP FROM dual" [] (0,1) :: IO [String]) >>= print

          prepareTable conn
          let insert = "INSERT INTO TEST_T_NAME(ID,NAME) VALUES(:id,:name)"
          _ <- execute conn insert [("id",return $ DataInt 0),("name",DataVarchar <$> fromByteString "test")]
          _ <- commitConnection conn
          (queryByPage conn "SELECT * FROM TEST_T_NAME" [] (0,1) :: IO [IdName]) >>= print
          mapM_ (\i -> execute conn insert [("id",return $ DataInt i),("name",fmap DataVarchar . fromByteString . BC.pack $ "test-" <> show i)]) [1..100]
          _ <- commitConnection conn
          (queryByPage conn "SELECT * FROM TEST_T_NAME" [] (10,20) :: IO [IdName]) >>= print

          [S{..}] <- queryByPage conn "select avg(milliseconds) as AVG from track" [] (0,1) :: IO [S]

          avg `shouldBe` (393599.2121039109334855837853268626891236 :: Scientific)



