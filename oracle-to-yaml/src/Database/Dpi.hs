{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
{-|

Module:      Database.Dpi
Copyright:   (c) Daniel YU
License:     BSD3
Maintainer:  leptonyu@gmail.com
Stability:   experimental
Portability: portable

FFI raw bindings to <https://oracle.github.io/odpi/doc ODPI-C>

@
import Database.Dpi

conf :: OracleConfig
conf = defaultOracle "username" "password" "localhost:1521/dbname"

{-# INLINE main #-}
main :: IO ()
main = do
  withContext $ \\cxt ->
    withPool cxt conf return $ \\pool ->
      withPoolConnection pool $ \\conn ->
        withStatement conn False "SELECT SYSDATE FROM DUAL" $ \\st -> do
          r <- executeStatement st ModeExecDefault
          f <- fetch st
          mapM (getQueryValue st) [1..r] >>= print
@

-}
module Database.Dpi
  ( module Database.Dpi
  , DpiException(..)
  , AuthMode(..)
  , ConnCloseMode(..)
  , CreateMode(..)
  , DeqMode(..)
  , DeqNavigation(..)
  , EventType(..)
  , ExecMode(..)
  , FetchMode(..)
  , MessageDeliveryMode(..)
  , MessageState(..)
  , NativeTypeNum(..)
  , OpCode(..)
  , OracleTypeNum(..)
  , PoolCloseMode(..)
  , PoolGetMode(..)
  , Purity(..)
  , ShutdownMode(..)
  , StartupMode(..)
  , StatementType(..)
  , SubscrGroupingClass(..)
  , SubscrGroupingType(..)
  , SubscrNamespace(..)
  , SubscrProtocol(..)
  , SubscrQOS(..)
  , Visibility(..)
  , PtrConn
  , PtrPool
  , PtrStmt
  , PtrVar
  , PtrLob
  , PtrObject
  , PtrObjectAttr
  , PtrObjectType
  , PtrRowid
  , PtrSubscr
  , PtrDeqOptions
  , PtrEnqOptions
  , PtrMsgProps
  , PtrContext
  , Data_AppContext(..)
  , Data_CommonCreateParams(..)
  , Data_ConnCreateParams(..)
  , Data(..)
  , DataValue(..)
  , Data_Bytes(..)
  , Data_Timestamp(..)
  , Data_IntervalDS(..)
  , Data_IntervalYM(..)
  , Data_DataTypeInfo(..)
  , Data_EncodingInfo(..)
  , Data_ErrorInfo(..)
  , Data_ObjectAttrInfo(..)
  , Data_ObjectTypeInfo(..)
  , Data_PoolCreateParams(..)
  , Data_QueryInfo(..)
  , Data_ShardingKeyColumn(..)
  , Data_StmtInfo(..)
  , Data_SubscrCreateParams(..)
  , Data_SubscrMessage(..)
  , Data_SubscrMessageQuery(..)
  , Data_SubscrMessageRow(..)
  , Data_SubscrMessageTable(..)
  , Data_VersionInfo(..)
  , PtrAppContext
  , PtrCommonCreateParams
  , PtrConnCreateParams
  , PtrData
  , PtrDataTypeInfo
  , PtrEncodingInfo
  , PtrErrorInfo
  , PtrObjectAttrInfo
  , PtrObjectTypeInfo
  , PtrPoolCreateParams
  , PtrQueryInfo
  , PtrShardingKeyColumn
  , PtrStmtInfo
  , PtrSubscrCreateParams
  , PtrSubscrMessage
  , PtrSubscrMessageQuery
  , PtrSubscrMessageRow
  , PtrSubscrMessageTable
  , PtrVersionInfo
  , getContextError
  ) where

import           Database.Dpi.Field
import           Database.Dpi.Internal
import           Database.Dpi.Prelude
import           Database.Dpi.Util

import           Control.Exception
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC

-- * Context Interface

-- $context
-- Context handles are the top level handles created by the library and are used for all
-- error handling as well as creating pools and standalone connections to the database.
-- The first call to ODPI-C by any application must be 'createContext' which will create the context
--  as well as validate the version used by the application. Context handles are destroyed by
-- using the function 'destroyContext'.

-- | Creates a new context for interaction with the library.
-- This is the first function that must be called and it must have completed successfully
-- before any other functions can be called, including in other threads.
{-# INLINE createContext #-}
createContext :: IO PtrContext
createContext = libContextCreate & inInt majorVersion & inInt minorVersion & go
  where
    {-# INLINE go #-}
    go :: (Ptr PtrContext -> PtrErrorInfo -> IO CInt) -> IO PtrContext
    go f = withPtrs $ \(pc,pe) -> do
      i <- f pc pe
      if isOk i then peek pc else peek pe >>= throw . ErrorInfoException

-- | Destroys the context that was earlier created with the function 'createContext'.
{-# INLINE destroyContext #-}
destroyContext :: PtrContext -> IO Bool
destroyContext p = runBool libContextDestroy (p, p)

-- | With Context, 'PtrContext' will be destroyed after run
withContext :: (PtrContext -> IO a) -> IO a
withContext = bracket createContext destroyContext

-- ** Information from Context

-- | Return information about the version of the Oracle Client that is being used.
{-# INLINE getClientVersion #-}
getClientVersion :: PtrContext -> IO Data_VersionInfo
getClientVersion p = libContextGetClientVersion p & outValue p peek


-- * Connection Interface
-- $connection
-- Connection handles are used to represent connections to the database.
-- These can be standalone connections created by calling the function 'createConnection'.
-- They can be closed by calling the function 'closeConnection' or releasing the last reference
-- to the connection by calling the function 'releaseConnection'.
-- Connection handles are used to create all handles other than session pools and context handles.


-- | Oracle Configuration
data OracleConfig = OracleConfig
  { username             :: ByteString -- ^ the name of the user used for authenticating the user
  , password             :: ByteString -- ^ the password to use for authenticating the user
  , connstr              :: ByteString -- ^ the connect string identifying the database to which a connection is to be established
  , connectionCreateMode :: CreateMode
  } deriving Show

-- | Default Configuration
defaultOracle
  :: ByteString
  -> ByteString
  -> ByteString
  -> OracleConfig
defaultOracle u p c = OracleConfig u p c ModeCreateDefault

-- | SQL String
type SQL = String

-- | Creates a standalone connection to a database or acquires a connection from a session pool and returns a reference to the connection.
{-# INLINE createConnection #-}
createConnection
  :: PtrContext -- ^ Context
  -> OracleConfig
  -> (Data_ConnCreateParams -> IO Data_ConnCreateParams) -- ^ custom 'Data_ConnCreateParams'
  -> IO PtrConn -- ^ a reference to the connection that is created.
  -- If a value is returned, a call to 'releaseConnection' must be made in order to release the reference.
  -- This should be done after the error information has been retrieved.
createConnection cxt OracleConfig{..} hccp =
  let hcmp ccp = return ccp { createMode = connectionCreateMode}
  in libConnCreate cxt
    & inStrLen username
    & inStrLen password
    & inStrLen connstr
    & inPtr (\c -> libContextInitCommonCreateParams cxt c >> peek c >>= hcmp >>= poke c)
    & inPtr (\c -> libContextInitConnCreateParams   cxt c >> peek c >>= hccp >>= poke c)
    & outCxtPtr (cxt,cxt)

-- | Closes the connection and makes it unusable for further activity. close connection, but not release resource, plese use 'releaseConnection' to release and close connection
{-# INLINE closeConnection #-}
closeConnection :: ConnCloseMode -> PtrConn -> IO Bool
closeConnection mode p = isOk <$> libConnClose (snd p) (fe mode) nullPtr 0

-- | Releases a reference to the connection. A count of the references to the connection is maintained
-- and when this count reaches zero, the memory associated with the connection is freed and
-- the connection is closed or released back to the session pool if that has not already taken place
--  using the function 'closeConnection'.
{-# INLINE releaseConnection #-}
releaseConnection :: PtrConn -> IO Bool
releaseConnection = runBool libConnRelease

-- | Commits the current active transaction.
{-# INLINE commitConnection #-}
commitConnection :: PtrConn -> IO Bool
commitConnection = runBool libConnCommit

-- | Rolls back the current active transaction.
{-# INLINE rollbackConnection #-}
rollbackConnection :: PtrConn -> IO Bool
rollbackConnection = runBool libConnRollback

-- | Pings the database to verify that the connection is still alive.
{-# INLINE pingConnection #-}
pingConnection :: PtrConn -> IO Bool
pingConnection = runBool libConnPing

-- | with connection
withConnection
  :: PtrContext -- ^ Context
  -> OracleConfig
  -> (Data_ConnCreateParams   -> IO Data_ConnCreateParams) -- ^ custom 'Data_ConnCreateParams'
  -> (PtrConn -> IO a) -- ^ action use connection
  -> IO a
withConnection p conf hccp2
  = bracket
      (createConnection p conf hccp2)
      (\c -> closeConnection ModeConnCloseDefault c `finally` releaseConnection c)

-- ** Transaction Interface

-- | Begins a distributed transaction using the specified transaction id (XID) made up of the formatId, transactionId and branchId.
{-# INLINE beginDistributedTransaction #-}
beginDistributedTransaction
  :: PtrConn -- ^ Connection
  -> Int64   -- ^  the identifier of the format of the XID. A value of -1 indicates that the entire XID is null.
  -> ByteString    -- ^  the global transaction id of the XID as a byte string. The maximum length permitted is 64 bytes.
  -> ByteString    -- ^ the branch id of the XID as a byte string. The maximum length permitted is 64 bytes.
  -> IO Bool
beginDistributedTransaction p formatId transId branchId
  = libConnBeginDistribTrans (snd p) (fromIntegral formatId)
    & inStrLen transId
    & inStrLen branchId
    & outBool

-- | Prepares a distributed transaction for commit.
-- This function should only be called after 'beginTransaction' is called and before 'commitConnection' is called.
{-# INLINE prepareDistributedTransaction #-}
prepareDistributedTransaction :: PtrConn -> IO Bool
prepareDistributedTransaction (cxt,p) = libConnPrepareDistribTrans p & outValue cxt peekBool

-- ** Information from Connection

-- | Returns the current schema that is being used by the connection.
{-# INLINE getCurrentSchema #-}
getCurrentSchema :: PtrConn -> IO ByteString
getCurrentSchema = runByteString libConnGetCurrentSchema

-- | Sets the current schema to be used on the connection.
-- This has the same effect as the SQL statement ALTER SESSION SET CURRENT_SCHEMA.
-- The value be changed when the next call requiring a round trip to the server is performed.
-- If the new schema name does not exist, the same error is returned as when the alter session statement is executed.
-- The new schema name is placed before database objects in statement that you execute that do not already have a schema.
{-# INLINE setCurrentSchema #-}
setCurrentSchema :: PtrConn -> ByteString -> IO Bool
setCurrentSchema = setString libConnSetCurrentSchema

-- | Returns the edition that is being used by the connection.
{-# INLINE getEdition #-}
getEdition :: PtrConn -> IO ByteString
getEdition = runByteString libConnGetEdition

-- | Returns the external name that is being used by the connection. This value is used when logging distributed transactions.
{-# INLINE getExternalName #-}
getExternalName :: PtrConn -> IO ByteString
getExternalName = runByteString libConnGetExternalName

-- | Sets the external name that is being used by the connection. This value is used when logging distributed transactions.
{-# INLINE setExternalName #-}
setExternalName :: PtrConn -> ByteString -> IO Bool
setExternalName = setString libConnSetExternalName

-- | Returns the internal name that is being used by the connection. This value is used when logging distributed transactions.
{-# INLINE getInternalName #-}
getInternalName :: PtrConn -> IO ByteString
getInternalName = runByteString libConnGetInternalName

-- | Sets the internal name that is being used by the connection. This value is used when logging distributed transactions.
{-# INLINE setInternalName #-}
setInternalName :: PtrConn -> ByteString -> IO Bool
setInternalName = setString libConnSetInternalName

-- | Returns the logical transaction id for the connection.
-- This value is used in Transaction Guard to determine if the last failed call was completed
  -- and if the transaction was committed using the procedure call dbms_app_cont.get_ltxid_outcome().
{-# INLINE getLTXID #-}
getLTXID :: PtrConn -> IO ByteString
getLTXID = runByteString libConnGetLTXID

-- | Returns the version information of the Oracle Database to which the connection has been made.
{-# INLINE getServerVersion #-}
getServerVersion :: PtrConn -> IO (ByteString, Data_VersionInfo)
getServerVersion (cxt,p) = libConnGetServerVersion p & out3Value cxt go
  where
    {-# INLINE go #-}
    go (pl,pv) = (,) <$> peekCStrLen pl <*> peek pv

-- | Looks up an object type by name in the database and returns a reference to it.
-- The reference should be released as soon as it is no longer needed.
{-# INLINE getObjectType #-}
getObjectType :: PtrConn -> ByteString -> IO PtrObjectType
getObjectType p name
  = libConnGetObjectType
    & inCxtPtr p
    & inStrLen name
    & outCxtPtr p

-- | Returns the encoding information used by the connection.
-- This will be equivalent to the values passed when the standalone connection or session pool was created,
-- or the values retrieved from the environment variables NLS_LANG and NLS_NCHAR.
{-# INLINE getEncodingInfo #-}
getEncodingInfo :: PtrConn -> IO Data_EncodingInfo
getEncodingInfo = runVar libConnGetEncodingInfo

-- | Returns the size of the statement cache, in number of statements.
{-# INLINE getStmtCacheSize #-}
getStmtCacheSize :: PtrConn -> IO Int
getStmtCacheSize = runInt libConnGetStmtCacheSize

-- | Sets the size of the statement cache.
{-# INLINE setStmtCacheSize #-}
setStmtCacheSize :: PtrConn -> Int -> IO Bool
setStmtCacheSize (_,p) size = libConnSetStmtCacheSize p & inInt size & outBool

-- | Sets the client info attribute on the connection.
-- This is one of the end-to-end tracing attributes that can be tracked in database views,
-- shown in audit trails and seen in tools such as Enterprise Manager.
{-# INLINE setClientInfo #-}
setClientInfo :: PtrConn -> ByteString -> IO Bool
setClientInfo = setString libConnSetClientInfo

-- | Sets the client identifier attribute on the connection.
-- This is one of the end-to-end tracing attributes that can be tracked in database views,
-- shown in audit trails and seen in tools such as Enterprise Manager.
{-# INLINE setClientIdentifier #-}
setClientIdentifier :: PtrConn -> ByteString -> IO Bool
setClientIdentifier = setString libConnSetClientIdentifier

-- | Sets the action attribute on the connection. This is one of the end-to-end tracing attributes
-- that can be tracked in database views, shown in audit trails and seen in tools such as Enterprise Manager.
{-# INLINE setAction #-}
setAction :: PtrConn -> ByteString -> IO Bool
setAction = setString libConnSetAction

-- | Sets the database operation attribute on the connection.
-- This is one of the end-to-end tracing attributes that can be tracked in database views,
-- shown in audit trails and seen in tools such as Enterprise Manager.
{-# INLINE setDbOp #-}
setDbOp :: PtrConn -> ByteString -> IO Bool
setDbOp = setString libConnSetDbOp

-- | Sets the module attribute on the connection.
-- This is one of the end-to-end tracing attributes that can be tracked in database views,
-- shown in audit trails and seen in tools such as Enterprise Manager.
{-# INLINE setModule #-}
setModule :: PtrConn -> ByteString -> IO Bool
setModule = setString libConnSetModule

-- | Returns the OCI service context handle in use by the connection.
{-# INLINE getHandler #-}
getHandler :: PtrConn -> IO (Ptr ())
getHandler = runVar libConnGetHandle

-- ** Connection Management

-- | Adds a reference to the connection.
-- This is intended for situations where a reference to the connection needs to be maintained independently of
  -- the reference returned when the connection was created.
{-# INLINE connectionAddRef #-}
connectionAddRef :: PtrConn -> IO Bool
connectionAddRef = runBool libConnAddRef

-- | Performs an immediate (asynchronous) termination of any currently executing function on the server associated with the connection.
{-# INLINE breakException #-}
breakException :: PtrConn -> IO Bool
breakException = runBool libConnBreakExecution

-- | Changes the password of the specified user.
{-# INLINE changePassword #-}
changePassword
  :: PtrConn -- ^ Connection
  -> ByteString    -- ^ the name of the user whose password is to be changed
  -> ByteString    -- ^ the old password of the user whose password is to be changed
  -> ByteString    -- ^ the new password of the user whose password is to be changed
  -> IO Bool
changePassword (_,p) username oldPassword newPassword
  = libConnChangePassword p
    & inStrLen username
    & inStrLen oldPassword
    & inStrLen newPassword
    & outBool

-- | Shuts down the database. This function must be called twice for the database to be shut down successfully.
-- After calling this function the first time, the SQL statements “alter database close normal”
-- and “alter database dismount” must be executed.
-- Once that is complete this function should be called again with the mode 'ModeShutdownFinal'
-- in order to complete the orderly shutdown of the database.
{-# INLINE shutdownDatabase #-}
shutdownDatabase
  :: PtrConn      -- ^ a reference to the connection to the database which is to be shut down.
  -- The connection needs to have been established at least with authorization mode set
  -- to 'ModeAuthSysdba' or 'ModeAuthSysoper'.
  -> ShutdownMode
  -> IO Bool
shutdownDatabase (_,p) sm = libConnShutdownDatabase p & inEnum sm & outBool

-- | Starts up a database.
{-# INLINE startupDatabase #-}
startupDatabase
  :: PtrConn     -- ^ a reference to the connection to the database which is to be started up.
  -- A connection like this can only be created with the authorization mode set to 'ModeAuthPrelim' along with
  -- one of 'ModeAuthSysdba' or 'ModeAuthSysoper'.
  -> StartupMode -- ^ one of the values from the enumeration 'StartupMode'
  -> IO Bool
startupDatabase (_,p) sm = libConnStartupDatabase p & inEnum sm & outBool

-- * ConnectionPool Interace
-- $pool
-- Pool handles are used to represent session pools.
-- They are created using the function 'createPool' and can be closed by calling the function 'closePool'
-- or releasing the last reference to the pool by calling the function 'releasePool'.
-- Pools can be used to create connections by calling the function 'acquiredConnection'.

-- | Acquires a connection from the pool and returns a reference to it. This reference should be released as soon as it is no longer needed.
{-# INLINE acquiredConnection #-}
acquiredConnection :: PtrPool -> IO PtrConn
acquiredConnection ps@(_,p) = libPoolAcquireConnection p nullPtr 0 nullPtr 0 nullPtr & outCxtPtr ps

-- | Adds a reference to the pool. This is intended for situations where a reference to
-- the pool needs to be maintained independently of the reference returned when the pool was created.
{-# INLINE poolAddRef #-}
poolAddRef :: PtrPool -> IO Bool
poolAddRef = runBool libPoolAddRef

-- | Creates a session pool which creates and maintains a group of stateless sessions to the database.
--  The main benefit of session pooling is performance since making a connection to the database is a time-consuming activity,
-- especially when the database is remote.
{-# INLINE createPool #-}
createPool
  :: PtrContext -- ^ Context
  -> OracleConfig
  -> (Data_PoolCreateParams -> IO Data_PoolCreateParams)
  -> IO PtrPool
createPool cxt OracleConfig{..} hpcp =
  let hcmp ccp = return ccp { createMode = connectionCreateMode}
  in libPoolCreate cxt
    & inStrLen username
    & inStrLen password
    & inStrLen connstr
    & inPtr (\c -> libContextInitCommonCreateParams cxt c >> peek c >>= hcmp >>= poke c)
    & inPtr (\c -> libContextInitPoolCreateParams   cxt c >> peek c >>= hpcp >>= poke c)
    & outCxtPtr (cxt,cxt)

-- | Closes the pool and makes it unusable for further activity.
{-# INLINE closePool #-}
closePool :: PtrPool -> PoolCloseMode -> IO Bool
closePool (_,p) mode = libPoolClose p & inEnum mode & outBool

-- | Releases a reference to the pool. A count of the references to the pool is maintained
-- and when this count reaches zero, the memory associated with the pool is freed
-- and the session pool is closed if that has not already taken place using the function 'closePool'.
{-# INLINE releasePool #-}
releasePool :: PtrPool -> IO Bool
releasePool = runBool libPoolRelease

-- | with pool
withPool
  :: PtrContext -- ^ Context
  -> OracleConfig
  -> (Data_PoolCreateParams -> IO Data_PoolCreateParams)
  -> (PtrPool -> IO a) -- ^ action use connection
  -> IO a
withPool p conf hpcp
  = bracket
      (createPool p conf hpcp)
      (\c -> closePool c ModePoolCloseDefault `finally` releasePool c)

-- | with pool provide a connection will released after action
{-# INLINE withPoolConnection #-}
withPoolConnection :: PtrPool -> (PtrConn -> IO a) -> IO a
withPoolConnection p = bracket (acquiredConnection p) releaseConnection

-- | Returns the number of sessions in the pool that are busy.
{-# INLINE getPoolBusyCount #-}
getPoolBusyCount :: PtrPool -> IO Int
getPoolBusyCount = runInt libPoolGetBusyCount

-- | Returns the encoding information used by the pool.
-- This will be equivalent to the values passed when the pool was created,
-- or the values retrieved from the environment variables NLS_LANG and NLS_NCHAR.
{-# INLINE getPoolEncodingInfo #-}
getPoolEncodingInfo :: PtrPool -> IO Data_EncodingInfo
getPoolEncodingInfo = runVar libPoolGetEncodingInfo

-- | Returns the mode used for acquiring or getting connections from the pool.
{-# INLINE getPoolMode #-}
getPoolMode :: PtrPool -> IO PoolGetMode
getPoolMode (cxt,p) = libPoolGetGetMode p & outValue cxt peekEnum

-- | Returns the maximum lifetime of all sessions in the pool, in seconds.
-- Sessions in the pool are terminated when this value has been reached,
-- but only when another session is released back to the pool.
{-# INLINE getPoolMaxLifetimeSession #-}
getPoolMaxLifetimeSession :: PtrPool -> IO Int
getPoolMaxLifetimeSession = runInt libPoolGetMaxLifetimeSession

-- | Returns the number of sessions in the pool that are open.
{-# INLINE getPoolOpenCount #-}
getPoolOpenCount :: PtrPool -> IO Int
getPoolOpenCount = runInt libPoolGetOpenCount

-- | Returns the default size of the statement cache for sessions in the pool, in number of statements.
{-# INLINE getPoolStmtCacheSize #-}
getPoolStmtCacheSize :: PtrPool -> IO Int
getPoolStmtCacheSize = runInt libPoolGetStmtCacheSize

-- | Returns the amount of time, in seconds, after which idle sessions in the pool are terminated,
-- but only when another session is released back to the pool.
{-# INLINE getPoolTimeout #-}
getPoolTimeout :: PtrPool -> IO Int
getPoolTimeout = runInt libPoolGetTimeout

-- | Returns the amount of time (in milliseconds) that the caller will wait for a session to become available
--  in the pool before returning an error.
--
-- @since <https://oracle.github.io/odpi/doc ODPI-C 2.4.0>
{-# INLINE getPoolWaitTimeout #-}
getPoolWaitTimeout :: PtrPool -> IO Int
getPoolWaitTimeout = runInt libPoolGetWaitTimeout

-- | Sets the mode used for acquiring or getting connections from the pool.
{-# INLINE setPoolGetMode #-}
setPoolGetMode :: PtrPool -> PoolGetMode -> IO Bool
setPoolGetMode (_,p) mode = libPoolSetGetMode p & inEnum mode & outBool

-- | Sets the maximum lifetime of all sessions in the pool, in seconds.
-- Sessions in the pool are terminated when this value has been reached,
-- but only when another session is released back to the pool.
{-# INLINE setPoolMaxLifetimeSession #-}
setPoolMaxLifetimeSession :: PtrPool -> Int -> IO Bool
setPoolMaxLifetimeSession (_,p) maxLifetimeSession
  = libPoolSetMaxLifetimeSession p
    & inInt maxLifetimeSession
    & outBool

-- | Sets the default size of the statement cache for sessions in the pool.
{-# INLINE setPoolStmtCacheSize #-}
setPoolStmtCacheSize :: PtrPool -> Int -> IO Bool
setPoolStmtCacheSize (_,p) stmtCacheSize
  = libPoolSetStmtCacheSize p
    & inInt stmtCacheSize
    & outBool

-- | Sets the amount of time, in seconds, after which idle sessions in the pool are terminated,
-- but only when another session is released back to the pool.
{-# INLINE setPoolTimeout #-}
setPoolTimeout :: PtrPool -> Int -> IO Bool
setPoolTimeout (_,p) timeout
  = libPoolSetTimeout p
    & inInt timeout
    & outBool

-- | Sets the amount of time (in milliseconds) that the caller should wait for a session to be available
--  in the pool before returning with an error.
--
-- @since <https://oracle.github.io/odpi/doc ODPI-C 2.4.0>
{-# INLINE setPoolWaitTimeout #-}
setPoolWaitTimeout :: PtrPool -> Int -> IO Bool
setPoolWaitTimeout (_,p) timeout
  = libPoolSetWaitTimeout p
    & inInt timeout
    & outBool

-- * Statement Interface
-- $statement
-- Statement handles are used to represent statements of all types (queries, DML, DDL and PL/SQL).
--  They are created by calling the function 'prepareStatement'.
-- They are also created implicitly when a variable of type 'OracleTypeStmt' is created.
--  Statement handles can be closed by calling the function 'closeStatement'
-- or by releasing the last reference to the statement by calling the function 'releaseStatement'.

-- | Returns a reference to a statement prepared for execution.
-- The reference should be released as soon as it is no longer needed.
{-# INLINE prepareStatement #-}
prepareStatement
  :: PtrConn    -- ^ Connection
  -> Bool       -- ^ a boolean indicating if the statement is scrollable or not.
    -- If it is scrollable, 'scrollStatement' can be used to reposition the cursor;
    -- otherwise, rows are retrieved in order from the statement until the rows are exhausted.
    -- This value is ignored for statements that do not refer to a query.
  -> SQL        -- ^ SQL String, not allow to use multi lines or semicolon as end of sql.
                -- use 'normalize' use normalize sql ByteString.
  -> IO PtrStmt
prepareStatement p scrollable sql
  = libConnPrepareStmt
    & inCxtPtr p
    & inBool scrollable
    & inStrLen sql
    & (\f -> f nullPtr 0)
    & outCxtPtr p

-- -- | Normalize SQL, replace newline characters with space characters. and remove semicolon in the end of sql
-- {-# INLINE normalize #-}
-- normalize :: SQL -> SQL
-- normalize = T.dropWhileEnd (==';')
--           . T.strip
--           . T.map (\c -> if c == '\n' || c == '\r' then ' ' else c)

-- {-# INLINE escapeName #-}
-- escapeName :: SQL -> SQL
-- escapeName name = "\"" <> T.replace "\"" "\"\"" name <> "\""

-- | Closes the statement and makes it unusable for further work immediately,
-- rather than when the reference count reaches zero.
{-# INLINE closeStatement #-}
closeStatement :: PtrStmt -> IO Bool
closeStatement (_, p) = isOk <$> libStmtClose p nullPtr 0

-- | Releases a reference to the statement. A count of the references to the statement is maintained
-- and when this count reaches zero, the memory associated with the statement is freed
-- and the statement is closed if that has not already taken place using the function 'closeStatement'.
{-# INLINE releaseStatement #-}
releaseStatement :: PtrStmt -> IO Bool
releaseStatement = runBool libStmtRelease

-- | with statement provide a prepared statement will released after action
withStatement
  :: PtrConn    -- ^ Connection
  -> Bool       -- ^ a boolean indicating if the statement is scrollable or not.
    -- If it is scrollable, 'scrollStatement' can be used to reposition the cursor;
    -- otherwise, rows are retrieved in order from the statement until the rows are exhausted.
    -- This value is ignored for statements that do not refer to a query.
  -> SQL        -- ^ SQL String, not allow to use multi lines or semicolon as end of sql.
                -- use 'normalize' use normalize sql ByteString.
  -> (PtrStmt -> IO a)
  -> IO a
withStatement p scrollable sql f
  = bracket
      (prepareStatement p scrollable sql)
      releaseStatement
      $ \s -> do a <- f s
                 a `seq` return a

-- | Scrolls the statement to the position in the cursor specified by the mode and offset.
{-# INLINE scrollStatement #-}
scrollStatement :: PtrStmt -> FetchMode -> Int -> Int -> IO Bool
scrollStatement (_,p) mode offset rowOffset = libStmtScroll p & inEnum mode & inInt offset & inInt rowOffset & outBool

-- | Adds a reference to the statement.
-- This is intended for situations where a reference to the statement needs to be maintained independently of
  -- the reference returned when the statement was created.
{-# INLINE statementAddRef #-}
statementAddRef :: PtrStmt -> IO Bool
statementAddRef = runBool libStmtAddRef

-- ** Statement Bind Vars

-- | Binds a variable to a named placeholder in the statement.
-- A reference to the variable is retained by the library and is released when the statement itself is released or a new variable is bound to the same name.
{-# INLINE bindByName #-}
bindByName
  :: PtrStmt -- ^ Statement
  -> ByteString    -- ^ a byte string in the encoding used for CHAR data giving the name of the placeholder which is to be bound.
  -> PtrVar  -- ^ a reference to the variable which is to be bound.
  -> IO Bool
bindByName (_,p) name (_,var)
  = libStmtBindByName p
    & inStrLen name
    & inVar var
    & outBool

-- | Binds a variable to a placeholder in the statement by position.
--  A reference to the variable is retained by the library and is released when the statement itself is released or a new variable is bound to the same position.
{-# INLINE bindByPosition #-}
bindByPosition
  :: PtrStmt -- ^ Statement
  -> Int     -- ^ the position which is to be bound.
             -- The position of a placeholder is determined by its location in the statement.
             -- Placeholders are numbered from left to right, starting from 1, and duplicate names do not count as additional placeholders.
  -> PtrVar  -- ^ a reference to the variable which is to be bound.
  -> IO Bool
bindByPosition (_,p) pos (_,var)
  = libStmtBindByPos p
    & inInt pos
    & inVar var
    & outBool

-- | Binds a value to a named placeholder in the statement without the need to create a variable directly.
-- One is created implicitly and released when the statement is released or a new value is bound to the same name.
{-# INLINE bindValueByName #-}
bindValueByName
  :: PtrStmt       -- ^ Statement
  -> ByteString    -- ^ a byte string in the encoding used for CHAR data giving the name of the placeholder which is to be bound.
  -> DataValue     -- ^ Value
                   -- Once the statement has been executed, this new variable will be released.
  -> IO Bool
bindValueByName (_,p) name dv = do
  (ntn, _, dt) <- newData dv
  libStmtBindValueByName p
    & inStrLen name
    & inEnum ntn
    & inVar dt
    & outBool

-- | Binds a value to a placeholder in the statement without the need to create a variable directly.
-- One is created implicitly and released when the statement is released or a new value is bound to the same position.
{-# INLINE bindValueByPosition #-}
bindValueByPosition
  :: PtrStmt       -- ^ Statement
  -> Int           -- ^  the position which is to be bound.
                   -- The position of a placeholder is determined by its location in the statement.
                   -- Placeholders are numbered from left to right, starting from 1,
                   -- and duplicate names do not count as additional placeholders.
  -> DataValue     -- ^ Value
                   -- Once the statement has been executed, this new variable will be released.
  -> IO Bool
bindValueByPosition (_,p) pos dv = do
  (ntn, _, dt) <- newData dv
  libStmtBindValueByPos p
    & inInt pos
    & inEnum ntn
    & inVar dt
    & outBool

-- | Defines the variable that will be used to fetch rows from the statement.
-- A reference to the variable will be retained until the next define is performed on the same position
-- or the statement is closed.
{-# INLINE define #-}
define
  :: PtrStmt -- ^ Statement
  -> Int     -- ^ the position which is to be defined. The first position is 1.
  -> PtrVar  -- ^ a reference to the variable which is to be used for fetching rows from the statement at the given position.
  -> IO Bool
define (_,p) pos (_,var)
  = libStmtDefine p
    & inInt pos
    & inVar var
    & outBool

-- | Defines the type of data that will be used to fetch rows from the statement.
-- This is intended for use with the function 'getQueryValue', when the default data type
-- derived from the column metadata needs to be overridden by the application.
-- Internally, a variable is created with the specified data type and size.
{-# INLINE defineValue #-}
defineValue
  :: PtrStmt       -- ^ Statement
  -> Int           -- ^ the position which is to be defined. The first position is 1.
  -> OracleTypeNum -- ^ the type of Oracle data that is to be used.
  -> NativeTypeNum -- ^ the type of native C data that is to be used.
  -> Int           -- ^ the maximum size of the buffer used for transferring data to/from Oracle.
                   -- This value is only used for variables transferred as byte strings.
                   -- Size is either in characters or bytes depending on the value of the sizeIsBytes parameter.
                   -- If the value is in characters, internally the value will be multipled by the maximum number of bytes
                   -- for each character and that value used instead when determining the necessary buffer size.
  -> Bool          -- ^ boolean value indicating if the size parameter refers to characters or bytes.
                   -- This flag is only used if the variable refers to character data.
  -> PtrObjectType -- ^ a reference to the object type of the object that is being bound or fetched.
                   -- This value is only used if the Oracle type is 'OracleTypeObject'.
  -> IO Bool
defineValue (_,p) pos otn ntn size isSizeInByte (_,ot)
  = libStmtDefineValue p
    & inInt pos
    & inEnum otn
    & inEnum ntn
    & inInt size
    & inBool isSizeInByte
    & inVar ot
    & outBool

-- | Returns the number of bind variables in the prepared statement.
-- In SQL statements this is the total number of bind variables whereas in PL/SQL statements
-- this is the count of the unique bind variables.
{-# INLINE getBindCount #-}
getBindCount :: PtrStmt -> IO Int
getBindCount = runInt libStmtGetBindCount

-- | Returns the names of the unique bind variables in the prepared statement.
{-# INLINE getBindNames #-}
getBindNames :: PtrStmt -> IO [ByteString]
getBindNames ps@(cxt,p) = do
  c <- getBindCount ps
  alloca $ \pn  ->
    allocaArray c $ \pan  ->
    alloca $ \panl -> do
      ok <- isOk <$> libStmtGetBindNames p pn pan panl
      if ok
        then do
          n  <- peek pn
          ac <- peekArray (fromIntegral n) pan
          al <- peek panl
          mapM (\cc -> B.packCStringLen (cc, fromIntegral al)) ac
        else throwContextError cxt

-- | Returns information about the statement.
{-# INLINE getStatementInfo #-}
getStatementInfo :: PtrStmt -> IO Data_StmtInfo
getStatementInfo = runVar libStmtGetInfo

-- | Gets the array size used for performing fetches.
{-# INLINE getFetchArraySize #-}
getFetchArraySize :: PtrStmt -> IO Int
getFetchArraySize = runInt libStmtGetFetchArraySize

-- | Sets the array size used for performing fetches.
-- All variables defined for fetching must have this many (or more) elements allocated for them.
-- The higher this value is the less network round trips are required to fetch rows from the database
-- but more memory is also required. A value of zero will reset the array size to
-- the default value of DPI_DEFAULT_FETCH_ARRAY_SIZE.
{-# INLINE setFetchArraySize #-}
setFetchArraySize :: PtrStmt -> Int -> IO Bool
setFetchArraySize (_,p) pos
  = libStmtSetFetchArraySize p
    & inInt pos
    & outBool

-- | Returns the next implicit result available from the last execution of the statement.
-- Implicit results are only available when both the client and server are 12.1 or higher.
{-# INLINE getImplicitResult #-}
getImplicitResult :: PtrStmt -> IO (Maybe PtrStmt)
getImplicitResult p@(cxt,_) = fmap (cxt,) <$> runMaybeVar libStmtGetImplicitResult p

-- | Returns the number of columns that are being queried.
{-# INLINE getNumberQueryColumns #-}
getNumberQueryColumns :: PtrStmt -> IO Int
getNumberQueryColumns = runInt libStmtGetNumQueryColumns

-- | Returns information about the column that is being queried.
{-# INLINE getQueryInfo #-}
getQueryInfo
  :: PtrStmt -- ^ Statement
  -> Int     -- ^ the position of the column whose metadata is to be retrieved. The first position is 1.
  -> IO Data_QueryInfo
getQueryInfo (cxt,p) pos
  = libStmtGetQueryInfo p
    & inInt pos
    & outValue cxt peek

-- | Returns the value of the column at the given position for the currently fetched row,
-- without needing to provide a variable. If the data type of the column needs to be overridden,
-- the function 'defineValue' can be called to specify a different type after executing
-- the statement but before fetching any data.
{-# INLINE getQueryValue #-}
getQueryValue
  :: PtrStmt -- ^ Statement
  -> Int     -- ^ the position of the column whose metadata is to be retrieved. The first position is 1.
  -> IO DataValue
getQueryValue ps@(cxt,p) pos
  = libStmtGetQueryValue p
    & inInt pos
    & out2Value cxt (go ps pos)
    where
      {-# INLINE go #-}
      go p' pos' (pt,pd) = do
        Data_QueryInfo{..} <- getQueryInfo p' pos'
        let Data_DataTypeInfo{..} = typeInfo
        t <- te <$> peek pt
        peek pd >>= _get oracleTypeNum t

-- ** Execute Statement

-- | Executes the statement using the bound values.
-- For queries this makes available metadata which can be acquired using the function 'getQueryInfo'.
-- For non-queries, out and in-out variables are populated with their values.
{-# INLINE executeStatement #-}
executeStatement :: PtrStmt -> ExecMode -> IO Int
executeStatement (cxt,p) mode = libStmtExecute p & inEnum mode & outValue cxt peekInt

{-# INLINE executeMany #-}
executeMany :: PtrStmt -> ExecMode -> Int -> IO Bool
executeMany (_,p) mode count
  = libStmtExecuteMany p
    & inEnum mode
    & inInt  count
    & outBool

-- | Fetches a single row from the statement. If the statement does not refer to a query an error is returned.
--  All columns that have not been defined prior to this call are implicitly defined using the metadata made available when the statement was executed.
{-# INLINE fetch #-}
fetch :: PtrStmt -> IO (Maybe PageOffset)
fetch (cxt,p)
  = libStmtFetch p
    & out2Value cxt go
    where
      {-# INLINE go #-}
      go (pf,pr) = do
        found <- toBool <$> peek pf
        if found then Just <$> peekInt pr else return Nothing

-- Index, RowNum
type PageOffset = Int64
type PageLimit  = Int64
type Page = (PageOffset, PageLimit)
type FetchRows a = PtrStmt -> Page -> IO a

-- | Returns the number of rows that are available in the buffers defined for the query.
--  If no rows are currently available in the buffers, an internal fetch takes place in order to populate them,
-- if rows are available. If the statement does not refer to a query an error is returned.
-- All columns that have not been defined prior to this call are implicitly defined using
-- the metadata made available when the statement was executed.
{-# INLINE fetchRows #-}
fetchRows
  :: PtrStmt -- ^ Statement
  -> Int     -- ^  the maximum number of rows to fetch. If the number of rows available exceeds this value only this number will be fetched.
  -> IO (Bool, [DataValue])
fetchRows ps@(cxt,p) maxRow
  = libStmtFetchRows p
    & inInt maxRow
    & out3Value cxt (go ps)
    where
      {-# INLINE go #-}
      go ps' ((pri,prf), pmr) = do
        index <- peekInt pri
        num   <- peekInt prf
        vs    <- fetch' ps' index num
        more  <- toBool <$> peek pmr
        return (more, vs)
      {-# INLINE fetch' #-}
      fetch' :: PtrStmt -> PageOffset -> PageLimit -> IO [DataValue]
      fetch' p' _ _ = do
        count <- getRowCount p'
        mapM (getQueryValue p') [1..count]

-- | Returns the number of rows affected by the last DML statement that was executed
-- or the number of rows currently fetched from a query. In all other cases 0 is returned.
{-# INLINE getRowCount #-}
getRowCount :: PtrStmt -> IO Int
getRowCount = runInt libStmtGetRowCount

-- | Returns an array of row counts affected by the last invocation of 'executeMany'
-- with the array DML rowcounts mode enabled.
-- This feature is only available if both client and server are at 12.1.
{-# INLINE getRowCounts #-}
getRowCounts :: PtrStmt -> IO [Int]
getRowCounts (cxt,p)
  = libStmtGetRowCounts p
    & out2Value cxt go
    where
      {-# INLINE go #-}
      go (pc, pac) = do
        c   <- peekInt pc
        pcs <- peekArray c pac
        mapM peekInt pcs

-- | Returns the id of the query that was just registered on the subscription by calling 'executeStatement'
-- on a statement prepared by calling 'prepareStatement'.
{-# INLINE getSubscrQueryId #-}
getSubscrQueryId :: PtrStmt -> IO Word64
getSubscrQueryId = runInt libStmtGetSubscrQueryId

{-# INLINE getBatchErrorCount #-}
getBatchErrorCount :: PtrStmt -> IO Int
getBatchErrorCount = runInt libStmtGetBatchErrorCount

{-# INLINE getBatchErrors #-}
getBatchErrors :: PtrStmt -> IO [Data_ErrorInfo]
getBatchErrors ps@(cxt,p) = do
  c <- getBatchErrorCount ps
  if c <= 0
    then return []
    else
      allocaArray c $ \par -> do
        ok <- libStmtGetBatchErrors p & inInt c & inVar par & outBool
        if ok then peekArray c par else throwContextError cxt

-- * Lob Interface
-- $lob
-- LOB handles are used to represent large objects (CLOB, BLOB, NCLOB, BFILE).
-- Both persistent and temporary large objects can be represented.
-- LOB handles can be created by calling the function 'newTempLob' or are created implicitly
-- when a variable of type 'OracleTypeClob', 'OracleTypeNclob', 'OracleTypeBlob' or 'OracleTypeBfile'
-- is created and are destroyed when the last reference is released by calling the function 'releaseLob'.
-- They are used for reading and writing data to the database in smaller pieces than is contained in the large object.

-- | Adds a reference to the LOB.
-- This is intended for situations where a reference to the LOB needs to be maintained independently of
-- the reference returned when the LOB was created.
{-# INLINE lobAddRef #-}
lobAddRef :: PtrLob -> IO Bool
lobAddRef = runBool libLobAddRef

-- | Returns a reference to a new temporary LOB which may subsequently be written and bound to a statement.
-- The reference should be released as soon as it is no longer needed.
{-# INLINE newTempLob #-}
newTempLob :: PtrConn -> OracleTypeNum -> IO PtrLob
newTempLob p otn
  = libConnNewTempLob
    & inCxtPtr p
    & inEnum otn
    & outCxtPtr p

-- | close lob
{-# INLINE closeLob #-}
closeLob :: PtrLob -> IO Bool
closeLob = runBool libLobClose

-- | Releases a reference to the LOB.
-- A count of the references to the LOB is maintained and when this count reaches zero,
-- the memory associated with the LOB is freed.
-- The LOB is also closed unless that has already taken place using the function 'closeLob'.
{-# INLINE releaseLob #-}
releaseLob :: PtrLob -> IO Bool
releaseLob = runBool libLobRelease

-- | Trims the data in the LOB so that it only contains the specified amount of data.
{-# INLINE trimLob #-}
trimLob :: PtrLob -> Int64 -> IO Bool
trimLob (_,p) size = libLobTrim p & inInt size & outBool

-- | Closes the LOB resource.
-- This should be done when a batch of writes has been completed so that the indexes associated with the LOB can be updated.
-- It should only be performed if a call to function 'openLobResource' has been performed.
{-# INLINE closeLobResource #-}
closeLobResource :: PtrLob -> IO Bool
closeLobResource = runBool libLobCloseResource

-- | Opens the LOB resource for writing.
-- This will improve performance when writing to the LOB in chunks
-- and there are functional or extensible indexes associated with the LOB.
-- If this function is not called, the LOB resource will be opened and closed for each write that is performed.
-- A call to the function 'closeLobResource' should be done before performing a call to the function 'commitConnection'.
{-# INLINE openLobResource #-}
openLobResource :: PtrLob -> IO Bool
openLobResource = runBool libLobOpenResource

-- | Returns a boolean value indicating if the LOB resource has been opened by making
-- a call to the function 'openLobResource' (1) or not (0).
{-# INLINE isLobResourceOpen #-}
isLobResourceOpen :: PtrLob -> IO Bool
isLobResourceOpen (cxt,p) = libLobGetIsResourceOpen p & outValue cxt peekBool

-- | Creates an independent copy of a LOB and returns a reference to the newly created LOB.
-- This reference should be released as soon as it is no longer needed.
{-# INLINE copyLob #-}
copyLob :: PtrLob -> IO PtrLob
copyLob p@(cxt,_)= (cxt,) <$> runVar libLobCopy p

-- | Returns the size of the buffer needed to hold the number of characters specified for a buffer of the type
-- associated with the LOB. If the LOB does not refer to a character LOB the value is returned unchanged.
{-# INLINE getLobBufferSize #-}
getLobBufferSize :: PtrLob -> Word64 -> IO Word64
getLobBufferSize (cxt,p) size
  = libLobGetBufferSize p
    & inInt size
    & outValue cxt peekInt

-- | Returns the chunk size of the internal LOB. Reading and writing to the LOB in multiples of this size will improve performance.
{-# INLINE getLobChunkSize #-}
getLobChunkSize :: PtrLob -> IO Int64
getLobChunkSize = runInt libLobGetChunkSize

-- | Returns the directory alias name and file name for a BFILE type LOB.
{-# INLINE getLobDirectoryAndFileName #-}
getLobDirectoryAndFileName :: PtrLob -> IO (FilePath, String)
getLobDirectoryAndFileName (cxt,p) = libLobGetDirectoryAndFileName p & out4Value cxt go
  where
    {-# INLINE go #-}
    go ((pd, pdlen), (pn, pnlen)) = do
      d    <- peek pd
      dlen <- peek pdlen
      n    <- peek pn
      nlen <- peek pnlen
      fp   <- BC.unpack <$> B.packCStringLen (d,fromIntegral dlen)
      name <- BC.unpack <$> B.packCStringLen (n,fromIntegral nlen)
      return (fp, name)

-- | Sets the directory alias name and file name for a BFILE type LOB.
{-# INLINE setLobDirectoryAndFileName #-}
setLobDirectoryAndFileName :: PtrLob -> (FilePath, String) -> IO Bool
setLobDirectoryAndFileName (_,p) (fp, name)
  = libLobSetDirectoryAndFileName p
    & inStrLen fp
    & inStrLen name
    & outBool

-- | Returns a boolean value indicating if the file referenced by the BFILE type LOB exists (1) or not (0).
{-# INLINE lobFileExists #-}
lobFileExists :: PtrLob -> IO Bool
lobFileExists (cxt,p) = libLobGetFileExists p & outValue cxt peekBool

-- | Returns the size of the data stored in the LOB.
-- For character LOBs the size is in characters; for binary LOBs the size is in bytes.
{-# INLINE getLobSize #-}
getLobSize :: PtrLob -> IO Int64
getLobSize = runInt libLobGetSize

-- | Replaces all of the data in the LOB with the contents of the provided buffer.
-- The LOB will first be cleared and then the provided data will be written.
{-# INLINE setLobFromBytes #-}
setLobFromBytes :: PtrLob -> ByteString -> IO Bool
setLobFromBytes (_,p) buff
  = libLobSetFromBytes p
    & inStrLen buff
    & outBool

type BufferSize = Int64

-- | Reads data from the LOB at the specified offset into the provided buffer.
{-# INLINE readLobBytes #-}
readLobBytes :: PtrLob -> Page -> BufferSize -> IO ByteString
readLobBytes (cxt,p) (offset, num) bufferSize
  = libLobReadBytes p
    & inInt offset
    & inInt num
    & uncurry
    & outValue' cxt get (set bufferSize)
    where
      set bs (_,pblen) = poke pblen (fromIntegral bs)
      get    (pb,pblen) = do
        pl <- peek pblen
        B.packCStringLen (pb,fromIntegral pl)

-- | Write data to the LOB at the specified offset using the provided buffer as the source.
-- If multiple calls to this function are planned, the LOB should first be opened using the function 'openLob'.
{-# INLINE writeLobBytes #-}
writeLobBytes :: PtrLob -> PageOffset -> ByteString -> IO Bool
writeLobBytes (_,p) size buff
  = libLobWriteBytes p
    & inInt size
    & inStrLen buff
    & outBool

-- * Object Interface
-- $object
-- Object handles are used to represent instances of the types created by the SQL command CREATE OR REPLACE TYPE.
-- They are created by calling the function 'createObject' or calling the function 'copyObject' or implicitly by
-- creating a variable of the type OracleTypeObject.
-- The are destroyed when the last reference is released by calling the function 'releaseObject'.

-- | Creates an object of the specified type and returns a reference to it.
-- This reference should be released as soon as it is no longer needed.
{-# INLINE createObject #-}
createObject :: PtrObjectType -> IO PtrObject
createObject p@(cxt,_)= (cxt,) <$> runVar libObjectTypeCreateObject p

-- | Releases a reference to the object.
-- A count of the references to the object is maintained and when this count reaches zero,
-- the memory associated with the object is freed.
{-# INLINE releaseObject #-}
releaseObject :: PtrObject -> IO Bool
releaseObject = runBool libObjectRelease

-- | Adds a reference to the object.
-- This is intended for situations where a reference to the object needs to be maintained independently of
-- the reference returned when the object was created.
{-# INLINE objectAddRef #-}
objectAddRef :: PtrObject -> IO Bool
objectAddRef = runBool libObjectAddRef

-- | Sets the value of the element found at the specified index.
{-# INLINE objectAppendElement #-}
objectAppendElement :: PtrObject -> NativeTypeNum -> PtrData -> IO Bool
objectAppendElement (_,p) ntn pd
  = libObjectAppendElement p
    & inEnum ntn
    & inVar pd
    & outBool

-- | Creates an independent copy of an object and returns a reference to the newly created object.
-- This reference should be released as soon as it is no longer needed.
{-# INLINE copyObject #-}
copyObject :: PtrObject -> IO PtrObject
copyObject  p@(cxt,_)= (cxt,) <$> runVar libObjectCopy p

-- | Trims a number of elements from the end of a collection.
{-# INLINE trimObject #-}
trimObject :: PtrObject -> Int -> IO Bool
trimObject (_,p) size
  = libObjectTrim p
    & inInt size
    & outBool

-- | Deletes an element from the collection.
-- Note that the position ordinals of the remaining elements are not changed.
-- The delete operation creates holes in the collection.
{-# INLINE objectDeleteElementByIndex #-}
objectDeleteElementByIndex :: PtrObject -> Int -> IO Bool
objectDeleteElementByIndex (_,p) pos
  = libObjectDeleteElementByIndex p
    & inInt pos
    & outBool

-- | Sets the value of one of the object’s attributes.
{-# INLINE setObjectAttributeValue #-}
setObjectAttributeValue :: PtrObject -> PtrObjectAttr -> DataValue -> IO Bool
setObjectAttributeValue (_,p) (_,poa) v = do
  (ntn, _, pd) <- newData v
  libObjectSetAttributeValue p poa & inEnum ntn & inVar pd & outBool

-- | Returns the value of one of the object’s attributes.
{-# INLINE getObjectAttributeValue #-}
getObjectAttributeValue :: PtrObject -> PtrObjectAttr -> NativeTypeNum -> OracleTypeNum -> IO DataValue
getObjectAttributeValue (cxt,p) (_,poa) ntn ot
  = libObjectGetAttributeValue p
    & inVar poa
    & inEnum ntn
    & outValue cxt (_get ot ntn)

-- Returns whether an element exists at the specified index.
{-# INLINE getObjectElementExistsByIndex #-}
getObjectElementExistsByIndex :: PtrObject -> Int -> IO Bool
getObjectElementExistsByIndex (cxt,p) ind
  = libObjectGetElementExistsByIndex p
    & inInt ind
    & outValue cxt peekBool

-- | Sets the value of the element found at the specified index.
{-# INLINE setObjectElementValueByIndex #-}
setObjectElementValueByIndex :: PtrObject -> Int -> DataValue -> IO Bool
setObjectElementValueByIndex (_,p) ind v = do
  (ntn, _, pd) <- newData v
  libObjectSetElementValueByIndex p & inInt ind & inEnum ntn & inVar pd & outBool

-- | Returns the value of the element found at the specified index.
{-# INLINE getObjectElementValueByIndex #-}
getObjectElementValueByIndex :: PtrObject -> Int -> NativeTypeNum -> OracleTypeNum -> IO DataValue
getObjectElementValueByIndex (cxt,p) pos ntn ot
  = libObjectGetElementValueByIndex p
    & inInt pos
    & inEnum ntn
    & outValue cxt (_get ot ntn)

-- | Returns the first index used in a collection.
{-# INLINE getObjectFirstIndex #-}
getObjectFirstIndex :: PtrObject -> IO (Maybe Int)
getObjectFirstIndex = runIndex libObjectGetFirstIndex

-- | Returns the last index used in a collection.
{-# INLINE getObjectLastIndex #-}
getObjectLastIndex :: PtrObject -> IO (Maybe Int)
getObjectLastIndex = runIndex libObjectGetLastIndex

-- | Returns the next index used in a collection following the specified index.
{-# INLINE getObjectNextIndex #-}
getObjectNextIndex :: PtrObject -> Int -> IO (Maybe Int)
getObjectNextIndex p ind = runIndex (flip libObjectGetNextIndex $ fromIntegral ind) p

-- | Returns the previous index used in a collection preceding the specified index.
{-# INLINE getObjectPrevIndex #-}
getObjectPrevIndex :: PtrObject -> Int -> IO (Maybe Int)
getObjectPrevIndex p ind = runIndex (flip libObjectGetPrevIndex $ fromIntegral ind) p

-- | Returns the number of elements in a collection.
{-# INLINE getObjectSize #-}
getObjectSize :: PtrObject -> IO Int
getObjectSize = runInt libObjectGetSize

-- ** ObjectAttr
-- $objectattr
-- Object attribute handles are used to represent the attributes of types such as those
-- created by the SQL command CREATE OR REPLACE TYPE.
-- They are created by calling the function 'getObjectTypeAttributes'
-- and are destroyed when the last reference is released by calling the function 'releaseObjectAttr'.

-- | Returns information about the attribute.
{-# INLINE getObjectAttrInfo #-}
getObjectAttrInfo :: PtrObjectAttr -> IO Data_ObjectAttrInfo
getObjectAttrInfo = runVar libObjectAttrGetInfo

-- | Adds a reference to the attribute.
-- This is intended for situations where a reference to the attribute needs to be maintained independently of
-- the reference returned when the attribute was created.
{-# INLINE objectAttrAddRef #-}
objectAttrAddRef :: PtrObjectAttr -> IO Bool
objectAttrAddRef = runBool libObjectAttrAddRef

-- | Releases a reference to the attribute.
-- A count of the references to the attribute is maintained and when this count reaches zero,
-- the memory associated with the attribute is freed.
{-# INLINE releaseObjectAttr #-}
releaseObjectAttr :: PtrObjectAttr -> IO Bool
releaseObjectAttr = runBool libObjectAttrRelease

-- ** ObjectType
-- $objecttype
-- Object type handles are used to represent types such as those created by the SQL command CREATE OR REPLACE TYPE.
-- They are created using the function 'getObjectType'
-- or implicitly when fetching from a column containing objects by calling the function 'getQueryInfo'.
--  Object types are also retrieved when used as attributes in another object by calling the function 'getObjectAttrInfo'
-- or as the element type of a collection by calling the function 'getObjectTypeInfo'.
-- They are destroyed when the last reference is released by calling the function 'releaseObjectType'.

-- | Adds a reference to the object type.
-- This is intended for situations where a reference to the object type needs to be maintained independently of
  -- the reference returned when the object type was created.
{-# INLINE objectTypeAddRef #-}
objectTypeAddRef :: PtrObjectType-> IO Bool
objectTypeAddRef = runBool libObjectTypeAddRef

-- | Releases a reference to the object type.
-- A count of the references to the object type is maintained and when this count reaches zero,
-- the memory associated with the object type is freed.
{-# INLINE releaseObjectType #-}
releaseObjectType :: PtrObjectType -> IO Bool
releaseObjectType = runBool libObjectTypeRelease

-- | Returns the list of attributes that belong to the object type.
{-# INLINE getObjectTypeAttributes #-}
getObjectTypeAttributes :: PtrObjectType -> Int -> IO PtrObjectAttr
getObjectTypeAttributes p num
  = libObjectTypeGetAttributes
    & inCxtPtr p
    & inInt num
    & outCxtPtr p

-- | Returns information about the object type.
{-# INLINE getObjectTypeInfo #-}
getObjectTypeInfo :: PtrObjectType -> IO Data_ObjectTypeInfo
getObjectTypeInfo = runVar libObjectTypeGetInfo

-- * Rowid Interface
-- $rowid
-- Rowid handles are used to represent the unique identifier of a row in the database.
-- They cannot be created or set directly but are created implicitly when a variable of type 'OracleTypeRowid' is created.
-- They are destroyed when the last reference is released by a call to the function 'releaseRowid'.

-- | Adds a reference to the rowid. This is intended for situations where a reference to the rowid needs to be maintained
-- independently of the reference returned when the rowid was created.
{-# INLINE rowidAddRef #-}
rowidAddRef :: PtrRowid -> IO Bool
rowidAddRef = runBool libRowidAddRef

-- | Releases a reference to the rowid.
-- A count of the references to the rowid is maintained and when this count reaches zero,
-- the memory associated with the rowid is freed.
{-# INLINE releaseRowid #-}
releaseRowid :: PtrRowid -> IO Bool
releaseRowid = runBool libRowidRelease

-- | Returns the sting (base64) representation of the rowid.
{-# INLINE rowidGetStringValue #-}
rowidGetStringValue :: PtrRowid -> IO ByteString
rowidGetStringValue (cxt,p) = libRowidGetStringValue p & out2Value cxt peekCStrLen

-- * Data Interface
-- $data
-- All of these functions are used for getting and setting the various members of the dpiData structure.
-- The members of the structure can be manipulated directly but some languages (such as Go) do not have the ability to
-- manipulate structures containing unions or the ability to process macros.
-- For this reason, none of these functions perform any error checking.
-- They are assumed to be replacements for direct manipulation of the various members of the structure.

-- | Returns the value of the data when the native type is 'NativeTypeBoolean'.
getBool :: PtrData -> IO Bool
getBool p = isOk <$> libDataGetBool p

-- | Sets the value of the data when the native type is 'NativeTypeBoolean'.
setBool :: PtrData -> Bool -> IO ()
setBool p v = libDataSetBool p & inBool v

-- | Returns a pointer to the value of the data when the native type is 'NativeTypeBytes'.
getBytes :: PtrData -> IO ByteString
getBytes p = libDataGetBytes p >>= peek >>= toByteString

-- | Sets the value of the data when the native type is 'NativeTypeBytes'.
-- Do not use this function when setting data for variables.
-- Instead, use the function 'setVarFromBytes'.
setBytes :: PtrData -> ByteString -> IO ()
setBytes p v = libDataSetBytes p & inStrLen v

-- | Returns the value of the data when the native type is 'NativeTypeDouble'.
getDouble :: PtrData -> IO Double
getDouble p = go <$> libDataGetDouble p
  where
    go (CDouble d) = d

-- | Sets the value of the data when the native type is 'NativeTypeDouble'.
setDouble :: PtrData -> Double -> IO ()
setDouble p v = libDataSetDouble p (CDouble v)

-- | Returns the value of the data when the native type is 'NativeTypeFloat'.
getFloat :: PtrData -> IO Float
getFloat p = go <$> libDataGetFloat p
  where
    go (CFloat d) = d

-- | Sets the value of the data when the native type is 'NativeTypeFloat'.
setFloat :: PtrData -> Float -> IO ()
setFloat p v = libDataSetFloat p (CFloat v)

-- | Returns the value of the data when the native type is 'NativeTypeInt64'.
getInt64 :: PtrData -> IO Int64
getInt64 p = ft <$> libDataGetInt64 p

-- | Sets the value of the data when the native type is 'NativeTypeInt64'.
setInt64 :: PtrData -> Int64 -> IO ()
setInt64 p v = libDataSetInt64 p (fromInteger . toInteger $ v)

-- | Returns the value of the data when the native type is 'NativeTypeUint64'.
getUint64 :: PtrData -> IO Word64
getUint64 p = ft <$> libDataGetUint64 p

-- | Sets the value of the data when the native type is 'NativeTypeUint64'.
setUint64 :: PtrData -> Word64 -> IO ()
setUint64 p v = libDataSetUint64 p (fromInteger . toInteger $ v)

-- | Returns the value of the data when the native type is 'NativeTypeIntervalDs'.
getIntervalDs :: PtrData -> IO Data_IntervalDS
getIntervalDs p = libDataGetIntervalDS p >>= peek

-- | Sets the value of the data when the native type is 'NativeTypeIntervalDs'.
setIntervalDs :: PtrData -> Data_IntervalDS -> IO ()
setIntervalDs p Data_IntervalDS{..} = libDataSetIntervalDS p days hours minutes seconds fseconds


-- | Returns the value of the data when the native type is 'NativeTypeIntervalYm'.
getIntervalYm :: PtrData -> IO Data_IntervalYM
getIntervalYm p = libDataGetIntervalYM p >>= peek

-- | Sets the value of the data when the native type is 'NativeTypeIntervalYm'.
setIntervalYm :: PtrData -> Data_IntervalYM -> IO ()
setIntervalYm p Data_IntervalYM{..} = libDataSetIntervalYM p years months

-- | Returns the value of the data when the native type is 'NativeTypeLob'.
getLob :: PtrData -> IO (Ptr DPI_Lob)
getLob = libDataGetLOB

-- | Sets the value of the data when the native type is 'NativeTypeLob'.
setLob :: PtrData -> Ptr DPI_Lob -> IO ()
setLob = libDataSetLOB

-- | Returns the value of the data when the native type is 'NativeTypeObject'.
getObject :: PtrData -> IO (Ptr DPI_Object)
getObject = libDataGetObject

-- | Sets the value of the data when the native type is 'NativeTypeObject'.
setObject :: PtrData -> Ptr DPI_Object -> IO ()
setObject = libDataSetObject

-- | Returns the value of the data when the native type is 'NativeTypeStmt'.
getStmt :: PtrData -> IO (Ptr DPI_Stmt)
getStmt = libDataGetStmt

-- | Sets the value of the data when the native type is 'NativeTypeStmt'.
setStmt :: PtrData -> Ptr DPI_Stmt -> IO ()
setStmt = libDataSetStmt

-- | Returns the value of the data when the native type is 'NativeTypeTimestamp'.
getTimestamp :: PtrData -> IO Data_Timestamp
getTimestamp p = libDataGetTimestamp p >>= peek

-- | Sets the value of the data when the native type is 'NativeTypeTimestamp'.
setTimestamp :: PtrData -> Data_Timestamp -> IO ()
setTimestamp p Data_Timestamp{..} = libDataSetTimestamp p year month day hour minute second fsecond tzHourOffset tzMinuteOffset

-- * Var Interface
-- $var
-- Variable handles are used to represent memory areas used for transferring data to and from the database.
-- They are created by calling the function 'newVar'.
--  They are destroyed when the last reference to the variable is released by calling the function 'releaseVar'.
--  They are bound to statements by calling the function 'bindByName' or the function 'bindByPosition'.
-- They can also be used for fetching data from the database by calling the function 'define'.

-- | Returns a reference to a new variable which can be used for binding data to a statement
--  or providing a buffer for querying data from the database.
-- The reference should be released as soon as it is no longer needed.
{-# INLINE newVar #-}
newVar :: PtrConn       -- ^ Connection
       -> OracleTypeNum -- ^ the type of Oracle data that is to be used.
       -> NativeTypeNum -- ^  the type of native C data that is to be used.
       -> Int           -- ^ the maximum number of rows that can be fetched or bound at one time from the database,
                        -- or the maximum number of elements that can be stored in a PL/SQL array.
       -> Int           -- ^ the maximum size of the buffer used for transferring data to/from Oracle.
                        -- This value is only used for variables transferred as byte strings.
                        -- Size is either in characters or bytes depending on the value of the sizeIsBytes parameter.
                        -- If the value is in characters, internally the value will be multipled by
                        -- the maximum number of bytes for each character and that value used instead
                        -- when determining the necessary buffer size.
       -> Bool          -- ^ boolean value indicating if the size parameter refers to characters or bytes.
                        -- This flag is only used if the variable refers to character data.
       -> Bool          -- ^  boolean value indicating if the variable refers to a PL/SQL array or simply to buffers used for binding or fetching data.
       -> PtrObjectType -- ^ a reference to the object type of the object that is being bound or fetched.
                        -- This value is only used if the Oracle type is 'OracleTypeObject'.
       -> IO (PtrVar, [PtrData])
newVar (cxt,p) otn ntn maxArraySize size sizeIsBytes isArray (_,oto)
  = libConnNewVar p
    & inEnum otn
    & inEnum ntn
    & inInt maxArraySize
    & inInt size
    & inBool sizeIsBytes
    & inBool isArray
    & inVar oto
    & out2Value cxt (go cxt)
    where
      {-# INLINE go #-}
      go c (pv,pd) = do
        v <- peek pv
        d <- peekArray (fromIntegral maxArraySize) pd
        return ((c,v), d)

-- | Adds a reference to the variable.
-- This is intended for situations where a reference to the variable needs to be maintained independently of
-- the reference returned when the variable was created.
{-# INLINE varAddRef #-}
varAddRef :: PtrVar -> IO Bool
varAddRef = runBool libVarAddRef

-- | Releases a reference to the variable.
-- A count of the references to the variable is maintained and when this count reaches zero,
-- the memory associated with the variable is freed.
{-# INLINE releaseVar #-}
releaseVar :: PtrVar -> IO Bool
releaseVar = runBool libVarRelease

-- | Copies the data from one variable to another variable.
{-# INLINE copyVar #-}
copyVar :: PtrVar -> Int -> PtrVar -> Int -> IO Bool
copyVar (_,p) toPos (_,from) fromPos
  = libVarCopyData p
    & inInt toPos
    & inVar from
    & inInt fromPos
    & outBool

-- | Returns a pointer to an array of dpiData structures used for transferring data to and from the database.
-- These structures are allocated by the variable itself and are made available when the variable is first
-- created using the function 'newVar'. If a DML returning statement is executed, however,
-- the number of allocated elements can change in addition to the memory location.
{-# INLINE getVarData #-}
getVarData :: PtrVar -> IO [Data]
getVarData pc =
  if version_3
    then getVarReturnedData pc 0
    else old pc
  where
    {-# INLINE go #-}
    go (pn, pd) = join $ peekArray <$> peekInt pn <*> peek pd
    {-# INLINE old #-}
    old (cxt,p) = libVarGetData p & out2Value cxt go

-- | Returns a pointer to an array of dpiData structures used for transferring data to and from the database.
-- These structures are allocated by the variable itself when a DML returning statement is executed and the variable is bound.
--
-- @since <https://oracle.github.io/odpi/doc ODPI-C 2.4.0>
{-# INLINE getVarReturnedData #-}
getVarReturnedData :: PtrVar -> Int -> IO [Data]
getVarReturnedData (cxt,p) pos = libVarGetReturnedData p & inInt pos & out2Value cxt go
  where
    {-# INLINE go #-}
    go (pn, pd) = join $ peekArray <$> peekInt pn <*> peek pd

-- | Returns the number of elements in a PL/SQL index-by table if the variable was created as an array
-- by the function 'newVar'.
-- If the variable is one of the output bind variables of a DML returning statement, however,
-- the value returned will correspond to the number of rows returned by the DML returning statement.
-- In all other cases, the value returned will be the number of elements the variable was created with.
{-# INLINE getVarElementsSize #-}
getVarElementsSize :: PtrVar -> IO Int
getVarElementsSize = runInt libVarGetNumElementsInArray

-- | Returns the size of the buffer used for one element of the array used for fetching/binding Oracle data
{-# INLINE getVarSizeInBytes #-}
getVarSizeInBytes :: PtrVar -> IO Int
getVarSizeInBytes = runInt libVarGetSizeInBytes

-- | Sets the variable value to the specified byte string.
-- In the case of the variable’s Oracle type being 'OracleTypeNumber',
-- the byte string is converted to an Oracle number during the call to this function.
{-# INLINE setVarFromBytes #-}
setVarFromBytes :: PtrVar -> Int -> ByteString -> IO Bool
setVarFromBytes (_,p) pos bytes
  = libVarSetFromBytes p
    & inInt pos
    & inStrLen bytes
    & outBool

-- | Sets the variable value to the specified LOB.
{-# INLINE setVarFromLob #-}
setVarFromLob :: PtrVar -> Int -> PtrLob -> IO Bool
setVarFromLob (_,p) pos (_,lob) = libVarSetFromLob p & inInt pos & inVar lob & outBool

-- | Sets the variable value to the specified object.
{-# INLINE setVarFromObject #-}
setVarFromObject :: PtrVar -> Int -> PtrObject -> IO Bool
setVarFromObject (_,p) pos (_,obj) = libVarSetFromObject p & inInt pos & inVar obj & outBool

-- | Sets the variable value to the specified rowid.
{-# INLINE setVarFromRowid #-}
setVarFromRowid :: PtrVar -> Int -> PtrRowid -> IO Bool
setVarFromRowid (_,p) pos (_,row) = libVarSetFromRowid p & inInt pos & inVar row & outBool

-- | Sets the variable value to the specified statement.
{-# INLINE setVarFromStatement #-}
setVarFromStatement :: PtrVar -> Int -> PtrStmt -> IO Bool
setVarFromStatement (_,p) pos (_,st) = libVarSetFromStmt p & inInt pos & inVar st & outBool

-- | Sets the number of elements in a PL/SQL index-by table.
{-# INLINE setVarNumberOfElements #-}
setVarNumberOfElements :: PtrVar -> Int -> IO Bool
setVarNumberOfElements (_,p) num = libVarSetNumElementsInArray p & inInt num & outBool


-- * DeqOptions Interface
-- $ deqOptions
-- Dequeue option handles are used to represent the options specified when dequeuing messages using advanced queueing.
-- They are created by calling the function 'newDeqOptions'
-- and are destroyed by releasing the last reference by calling the function 'releaseDeqOptions'.

-- | Returns a reference to a new set of dequeue options, used in dequeuing objects from a queue.
-- The reference should be released as soon as it is no longer needed.
{-# INLINE newDeqOptions #-}
newDeqOptions :: PtrConn -> IO PtrDeqOptions
newDeqOptions p = libConnNewDeqOptions & inCxtPtr p & outCxtPtr p

-- | Dequeues a message from a queue.
{-# INLINE deqObject #-}
deqObject
  :: PtrConn         -- ^ a reference to the connection from which the message is to be dequeued
  -> ByteString            -- ^ the name of the queue from which the message is to be dequeued
  -> PtrDeqOptions   -- ^ a reference to the dequeue options that should be used when dequeuing the message from the queue.
  -> PtrMsgProps     -- ^ a reference to the message properties that will be populated with information from the message that is dequeued.
  -> PtrObject       -- ^ a reference to the object which will be populated with the message that is dequeued.
  -> IO (Maybe ByteString) -- ^ a pointer to a byte string which will be populated with the id of the message that is dequeued
deqObject (cxt,p) queueName options props payload
  = libConnDeqObject p
    & inStrLen queueName
    & inCxtPtr options
    & inCxtPtr props
    & inCxtPtr payload
    & out2Value cxt peekMaybeCStrLen

-- | Releases a reference to the dequeue options.
-- A count of the references to the dequeue options is maintained and when this count reaches zero,
-- the memory associated with the options is freed.
{-# INLINE releaseDeqOptions #-}
releaseDeqOptions :: PtrDeqOptions -> IO Bool
releaseDeqOptions = runBool libDeqOptionsRelease

-- | Adds a reference to the dequeue options.
-- This is intended for situations where a reference to the dequeue options needs to be maintained independently of
  -- the reference returned when the handle was created.
{-# INLINE deqOptionsAddRef #-}
deqOptionsAddRef :: PtrDeqOptions -> IO Bool
deqOptionsAddRef = runBool libDeqOptionsAddRef

-- | Returns the condition that must be satisfied in order for a message to be dequeued.
-- See function 'setDeqOptionsCondition' for more information.
{-# INLINE getDeqOptionsCondition #-}
getDeqOptionsCondition :: PtrDeqOptions -> IO ByteString
getDeqOptionsCondition = runByteString libDeqOptionsGetCondition

-- | Sets the condition which must be true for messages to be dequeued.
-- The condition must be a valid boolean expression similar to the where clause of a SQL query.
-- The expression can include conditions on message properties, user data properties and PL/SQL or SQL functions.
-- User data properties must be prefixed with tab.user_data as a qualifier to indicate
-- the specific column of the queue table that stores the message payload.
{-# INLINE setDeqOptionsCondition #-}
setDeqOptionsCondition :: PtrDeqOptions -> ByteString -> IO Bool
setDeqOptionsCondition = setString libDeqOptionsSetCondition

-- | Returns the name of the consumer that is dequeuing messages.
-- See function 'setDeqOptionsConsumerName' for more information.
{-# INLINE getDeqOptionsConsumerName #-}
getDeqOptionsConsumerName :: PtrDeqOptions -> IO ByteString
getDeqOptionsConsumerName = runByteString libDeqOptionsGetConsumerName

-- | Sets the name of the consumer which will be dequeuing messages.
-- This value should only be set if the queue is set up for multiple consumers.
{-# INLINE setDeqOptionsConsumerName #-}
setDeqOptionsConsumerName :: PtrDeqOptions -> ByteString -> IO Bool
setDeqOptionsConsumerName = setString libDeqOptionsSetConsumerName

-- | Returns the correlation of the message to be dequeued.
-- See function 'setDeqOptionsCorrelation' for more information.
{-# INLINE getDeqOptionsCorrelation #-}
getDeqOptionsCorrelation :: PtrDeqOptions -> IO ByteString
getDeqOptionsCorrelation = runByteString libDeqOptionsGetCorrelation

-- | Sets the correlation of the message to be dequeued.
-- Special pattern matching characters such as the percent sign (%) and the underscore (_) can be used.
-- If multiple messages satisfy the pattern, the order of dequeuing is undetermined.
{-# INLINE setDeqOptionsCorrelation #-}
setDeqOptionsCorrelation :: PtrDeqOptions -> ByteString -> IO Bool
setDeqOptionsCorrelation = setString libDeqOptionsSetCorrelation

-- | Returns the mode that is to be used when dequeuing messages.
{-# INLINE getDeqOptionsMode #-}
getDeqOptionsMode :: PtrDeqOptions -> IO DeqMode
getDeqOptionsMode (cxt,p) = libDeqOptionsGetMode p & outValue cxt peekEnum

-- | Sets the mode that is to be used when dequeuing messages.
{-# INLINE setDeqOptionsMode #-}
setDeqOptionsMode :: PtrDeqOptions -> DeqMode -> IO Bool
setDeqOptionsMode (_,p) mdm = libDeqOptionsSetMode p & inEnum mdm & outBool

-- | Returns the identifier of the specific message that is to be dequeued.
{-# INLINE getDeqOptionsMsgId #-}
getDeqOptionsMsgId :: PtrDeqOptions -> IO ByteString
getDeqOptionsMsgId = runByteString libDeqOptionsGetMsgId

-- | Sets the identifier of the specific message to be dequeued.
{-# INLINE setDeqOptionsMsgId #-}
setDeqOptionsMsgId :: PtrDeqOptions -> ByteString -> IO Bool
setDeqOptionsMsgId = setString libDeqOptionsSetMsgId

-- | Returns the position of the message that is to be dequeued.
{-# INLINE getDeqOptionsNavigation #-}
getDeqOptionsNavigation :: PtrDeqOptions -> IO DeqNavigation
getDeqOptionsNavigation (cxt,p) = libDeqOptionsGetNavigation p & outValue cxt peekEnum

-- | Sets the position in the queue of the message that is to be dequeued.
{-# INLINE setDeqOptionsNavigation #-}
setDeqOptionsNavigation :: PtrDeqOptions -> DeqNavigation -> IO Bool
setDeqOptionsNavigation (_,p) mdm = libDeqOptionsSetNavigation p & inEnum mdm & outBool

-- | Returns the transformation of the message to be dequeued.
-- See function 'setDeqOptionsTransformation' for more information.
{-# INLINE getDeqOptionsTransformation #-}
getDeqOptionsTransformation :: PtrDeqOptions -> IO ByteString
getDeqOptionsTransformation = runByteString libDeqOptionsGetTransformation

-- | Sets the transformation of the message to be dequeued.
-- The transformation is applied after the message is dequeued but before it is returned to the application.
-- It must be created using DBMS_TRANSFORM.
{-# INLINE setDeqOptionsTransformation #-}
setDeqOptionsTransformation :: PtrDeqOptions -> ByteString -> IO Bool
setDeqOptionsTransformation = setString libDeqOptionsSetTransformation

-- | Returns whether the message being dequeued is part of the current transaction or constitutes a transaction on its own.
{-# INLINE getDeqOptionsVisibility #-}
getDeqOptionsVisibility :: PtrDeqOptions -> IO Visibility
getDeqOptionsVisibility (cxt,p) = libDeqOptionsGetVisibility p & outValue cxt peekEnum

-- | Sets whether the message being dequeued is part of the current transaction or constitutes a transaction on its own.
{-# INLINE setDeqOptionsVisibility #-}
setDeqOptionsVisibility :: PtrDeqOptions -> Visibility -> IO Bool
setDeqOptionsVisibility (_,p) mdm = libDeqOptionsSetVisibility p & inEnum mdm & outBool

-- | Returns the time to wait, in seconds, for a message matching the search criteria.
-- See function 'setDeqOptionsWait' for more information.
{-# INLINE getDeqOptionsWait #-}
getDeqOptionsWait :: PtrDeqOptions -> IO Int
getDeqOptionsWait = runInt libDeqOptionsGetWait

-- | Set the time to wait, in seconds, for a message matching the search criteria.
{-# INLINE setDeqOptionsWait #-}
setDeqOptionsWait :: PtrDeqOptions -> Int -> IO Bool
setDeqOptionsWait (_,p) wait = libDeqOptionsSetWait p & inInt wait & outBool

-- | Sets the message delivery mode that is to be used when dequeuing messages.
{-# INLINE setDeqOptionsDeliveryMode #-}
setDeqOptionsDeliveryMode :: PtrDeqOptions -> MessageDeliveryMode -> IO Bool
setDeqOptionsDeliveryMode (_,p) mdm = libDeqOptionsSetDeliveryMode p & inEnum mdm & outBool

-- * EnqOptions Interface
-- $enq
-- Enqueue option handles are used to represent the options specified when enqueuing messages using advanced queueing.
-- They are created by calling the function 'newEnqOptions'
-- and are destroyed by releasing the last reference by calling the function 'releaseEnqOptions'.

-- | Returns a reference to a new set of enqueue options, used in enqueuing objects into a queue.
-- The reference should be released as soon as it is no longer needed.
{-# INLINE newEnqOptions #-}
newEnqOptions :: PtrConn -> IO PtrEnqOptions
newEnqOptions p = libConnNewEnqOptions & inCxtPtr p & outCxtPtr p

-- | Enqueues a message to a queue.
{-# INLINE enqObject #-}
enqObject
  :: PtrConn         -- ^ a reference to the connection from which the message is to be enqueued
  -> ByteString            -- ^ the name of the queue from which the message is to be enqueued
  -> PtrEnqOptions   -- ^ a reference to the enqueue options that should be used when enqueued the message from the queue.
  -> PtrMsgProps     -- ^ a reference to the message properties that will be populated with information from the message that is enqueued.
  -> PtrObject       -- ^ a reference to the object which will be populated with the message that is enqueued.
  -> IO (Maybe ByteString) -- ^ a pointer to a byte string which will be populated with the id of the message that is enqueued
enqObject (cxt,p) queueName (_,options) (_,props) (_,payload)
  = libConnEnqObject p
    & inStrLen queueName
    & inVar options
    & inVar props
    & inVar payload
    & out2Value cxt peekMaybeCStrLen

-- | Adds a reference to the enqueue options.
-- This is intended for situations where a reference to the enqueue options needs to be maintained independently of
-- the reference returned when the handle was created.
{-# INLINE enqOptionsAddRef #-}
enqOptionsAddRef :: PtrEnqOptions -> IO Bool
enqOptionsAddRef = runBool libEnqOptionsAddRef

-- | Releases a reference to the enqueue options.
-- A count of the references to the enqueue options is maintained and when this count reaches zero,
-- the memory associated with the options is freed.
{-# INLINE releaseEnqOptions #-}
releaseEnqOptions :: PtrEnqOptions -> IO Bool
releaseEnqOptions = runBool libEnqOptionsRelease

-- | Returns the transformation of the message to be enqueued.
-- See function 'setEnqOptionsTransformation' for more information.
{-# INLINE getEnqOptionsTransformation #-}
getEnqOptionsTransformation :: PtrEnqOptions -> IO ByteString
getEnqOptionsTransformation = runByteString libEnqOptionsGetTransformation

-- | Sets the transformation of the message to be enqueued.
-- The transformation is applied after the message is enqueued but before it is returned to the application.
-- It must be created using DBMS_TRANSFORM.
{-# INLINE setEnqOptionsTransformation #-}
setEnqOptionsTransformation :: PtrEnqOptions -> ByteString -> IO Bool
setEnqOptionsTransformation = setString libEnqOptionsSetTransformation

-- | Returns whether the message being enqueued is part of the current transaction or constitutes a transaction on its own.
{-# INLINE getEnqOptionsVisibility #-}
getEnqOptionsVisibility :: PtrEnqOptions -> IO Visibility
getEnqOptionsVisibility (cxt,p) = libEnqOptionsGetVisibility p & outValue cxt peekEnum

-- | Sets whether the message being enqueued is part of the current transaction or constitutes a transaction on its own.
{-# INLINE setEnqOptionsVisibility #-}
setEnqOptionsVisibility :: PtrEnqOptions -> Visibility -> IO Bool
setEnqOptionsVisibility (_,p) mdm = libEnqOptionsSetVisibility p & inEnum mdm & outBool

-- | Sets the message delivery mode that is to be used when enqueuing messages.
{-# INLINE setEnqOptionsDeliveryMode #-}
setEnqOptionsDeliveryMode :: PtrEnqOptions -> MessageDeliveryMode -> IO Bool
setEnqOptionsDeliveryMode (_,p) mdm = libEnqOptionsSetDeliveryMode p & inEnum mdm & outBool


-- * MsgProps Interface
-- $ msg
-- Message properties handles are used to represent the properties of messages that are enqueued
-- and dequeued using advanced queuing.
-- They are created by calling the function 'newMsgProps' and are destroyed by releasing the last reference
-- by calling the function 'releaseMsgProps'.

-- | Returns a reference to a new set of message properties, used in enqueuing and dequeuing objects in a queue.
-- The reference should be released as soon as it is no longer needed.
{-# INLINE newMsgProps #-}
newMsgProps :: PtrConn -> IO PtrMsgProps
newMsgProps p = libConnNewMsgProps & inCxtPtr p & outCxtPtr p

-- | Adds a reference to the message properties.
-- This is intended for situations where a reference to the message properties
-- needs to be maintained independently of the reference returned when the handle was created.
{-# INLINE msgPropsAddRef #-}
msgPropsAddRef :: PtrMsgProps -> IO Bool
msgPropsAddRef = runBool libMsgPropsAddRef

-- | Releases a reference to the message properties.
--  A count of the references to the message properties is maintained and when this count reaches zero,
-- the memory associated with the properties is freed.
{-# INLINE releaseMsgProps #-}
releaseMsgProps :: PtrMsgProps -> IO Bool
releaseMsgProps = runBool libMsgPropsRelease

-- | Returns the correlation supplied by the producer when the message was enqueued.
{-# INLINE getMsgPropsCorrelation #-}
getMsgPropsCorrelation :: PtrMsgProps -> IO ByteString
getMsgPropsCorrelation = runByteString libMsgPropsGetCorrelation

-- | Sets the correlation of the message to be dequeued.
-- Special pattern matching characters such as the percent sign (%) and the underscore (_) can be used.
-- If multiple messages satisfy the pattern, the order of dequeuing is undetermined.
{-# INLINE setMsgPropsCorrelation #-}
setMsgPropsCorrelation :: PtrMsgProps -> ByteString -> IO Bool
setMsgPropsCorrelation = setString libMsgPropsSetCorrelation

-- | Returns the number of attempts that have been made to dequeue a message.
{-# INLINE getMsgPropsNumAttempts #-}
getMsgPropsNumAttempts :: PtrMsgProps -> IO Int
getMsgPropsNumAttempts = runInt libMsgPropsGetNumAttempts

-- | Returns the number of seconds the enqueued message will be delayed.
{-# INLINE getMsgPropsDelay #-}
getMsgPropsDelay :: PtrMsgProps -> IO Int
getMsgPropsDelay = runInt libMsgPropsGetDelay

-- | Sets the number of seconds to delay the message before it can be dequeued.
-- Messages enqueued with a delay are put into the 'MsgStateWaiting' state.
-- When the delay expires the message is put into the 'MsgStateReady' state.
-- Dequeuing directly by message id overrides this delay specification.
-- Note that delay processing requires the queue monitor to be started.
{-# INLINE setMsgPropsDelay #-}
setMsgPropsDelay :: PtrMsgProps -> Int -> IO Bool
setMsgPropsDelay (_,p) delay = libMsgPropsSetDelay p & inInt delay & outBool

-- | Returns the mode that was used to deliver the message.
{-# INLINE getMsgPropsDeliveryMode #-}
getMsgPropsDeliveryMode :: PtrMsgProps -> IO MessageDeliveryMode
getMsgPropsDeliveryMode (cxt,p) = libMsgPropsGetDeliveryMode p & outValue cxt peekEnum

-- | Returns the time that the message was enqueued.
{-# INLINE getMsgPropsEnqTime #-}
getMsgPropsEnqTime :: PtrMsgProps -> IO Data_Timestamp
getMsgPropsEnqTime = runVar libMsgPropsGetEnqTime

-- | Returns the name of the queue to which the message is moved if it cannot be processed successfully.
-- See function 'setMsgPropsExceptionQ' for more information.
{-# INLINE getMsgPropsExceptionQ #-}
getMsgPropsExceptionQ :: PtrMsgProps -> IO ByteString
getMsgPropsExceptionQ = runByteString libMsgPropsGetExceptionQ

-- | Sets the name of the queue to which the message is moved if it cannot be processed successfully.
-- Messages are moved if the number of unsuccessful dequeue attempts has reached the maximum allowed number
-- or if the message has expired. All messages in the exception queue are in the 'MsgStateExpired' state.
{-# INLINE setMsgPropsExceptionQ #-}
setMsgPropsExceptionQ :: PtrMsgProps -> ByteString -> IO Bool
setMsgPropsExceptionQ = setString libMsgPropsSetExceptionQ

-- | Returns the number of seconds the message is available to be dequeued.
-- See function 'setMsgPropsExpiration' for more information.
{-# INLINE getMsgPropsExpiration #-}
getMsgPropsExpiration :: PtrMsgProps -> IO Int
getMsgPropsExpiration = runInt libMsgPropsGetExpiration

-- | Sets the number of seconds the message is available to be dequeued.
-- This value is an offset from the delay.
-- Expiration processing requires the queue monitor to be running.
-- Until this time elapses, the messages are in the queue in the state 'MsgStateReady'.
-- After this time elapses messages are moved to the exception queue in the 'MsgStateExpired' state.
{-# INLINE setMsgPropsExpiration #-}
setMsgPropsExpiration :: PtrMsgProps -> Int -> IO Bool
setMsgPropsExpiration (_,p) delay = libMsgPropsSetExpiration p & inInt delay & outBool

-- | Returns the id of the message in the last queue that generated this message.
-- See function 'setMsgPropsOriginalMsgId' for more information.
{-# INLINE getMsgPropsOriginalMsgId #-}
getMsgPropsOriginalMsgId :: PtrMsgProps -> IO ByteString
getMsgPropsOriginalMsgId = runByteString libMsgPropsGetOriginalMsgId

-- | Sets the id of the message in the last queue that generated this message.
{-# INLINE setMsgPropsOriginalMsgId #-}
setMsgPropsOriginalMsgId :: PtrMsgProps -> ByteString -> IO Bool
setMsgPropsOriginalMsgId = setString libMsgPropsSetOriginalMsgId

-- | Returns the priority assigned to the message.
-- See function 'setMsgPropsPriority' for more information.
{-# INLINE getMsgPropsPriority #-}
getMsgPropsPriority :: PtrMsgProps -> IO Int
getMsgPropsPriority = runInt libMsgPropsGetPriority

-- | Sets the priority assigned to the message. A smaller number indicates a higher priority.
-- The priority can be any number, including negative numbers.
{-# INLINE setMsgPropsPriority #-}
setMsgPropsPriority :: PtrMsgProps -> Int -> IO Bool
setMsgPropsPriority (_,p) delay = libMsgPropsSetPriority p & inInt delay & outBool

-- | Returns the state of the message at the time of dequeue.
{-# INLINE getMsgPropsState #-}
getMsgPropsState :: PtrMsgProps -> IO MessageState
getMsgPropsState (cxt,p) = libMsgPropsGetState p & outValue cxt peekEnum

-- * Subscr Interface
-- $subscr
-- Subscription handles are used to represent subscriptions to events such as continuous query notification
-- and object change notification.
-- They are created by calling the function 'newSubscr'
-- and are destroyed by calling the function 'closeSubscr' or releasing the last reference by calling
-- the function 'releaseSubscr'.

-- | Returns a reference to a subscription which is used for requesting notifications of changes on tables or queries
-- that are made in the database. The reference should be released as soon as it is no longer needed.
{-# INLINE newSubscr #-}
{-# DEPRECATED newSubscr "deprecated in 3.x, use subscribe in stead" #-}
newSubscr
  :: PtrConn
  -> (Data_SubscrCreateParams -> Data_SubscrCreateParams)
  -> IO PtrSubscr
newSubscr (cxt,p)  hcmp
  = libConnNewSubscription p
  & inPtr (\c -> libContextInitSubscrCreateParams cxt c >> peek c >>= poke c . hcmp)
  & out2Value cxt (go cxt)
  where
    {-# INLINE go #-}
    go c (p',_) = (c,) <$> peek p'

-- | Returns a reference to a subscription which is used for requesting notifications of events that take place in the database.
-- Events that are supported are changes on tables or queries (continuous query notification)
-- and the availability of messages to dequeue (advanced queuing).
-- The reference should be released as soon as it is no longer needed.
--
-- @since <https://oracle.github.io/odpi/doc ODPI-C 2.4.0>
subscribe
  :: PtrConn
  -> (Data_SubscrCreateParams -> Data_SubscrCreateParams)
  -> IO PtrSubscr
subscribe (cxt,p) hcmp
  = libConnSubscribe p
    & inPtr (\c -> libContextInitSubscrCreateParams cxt c >> peek c >>= poke c . hcmp)
    & outValue cxt (peekWithCxt cxt)

-- | Unubscribes from the events that were earlier subscribed to via the function 'subscribe'.
-- Once this function completes successfully no further notifications will be sent for this subscription.
--  Note that this method does not generate a notification either.
--
-- @since <https://oracle.github.io/odpi/doc ODPI-C 2.4.0>
unsubscribe :: PtrConn -> PtrSubscr -> IO Bool
unsubscribe (_,p) (_,ps) = isOk <$> libConnUnsubscribe p ps

-- | Adds a reference to the subscription.
-- This is intended for situations where a reference to the subscription
-- needs to be maintained independently of the reference returned when the subscription was created.
{-# INLINE subscrAddRef #-}
subscrAddRef :: PtrSubscr -> IO Bool
subscrAddRef = runBool libSubscrAddRef

-- | Closes the subscription now, rather than when the last reference is released.
-- This deregisters it so that notifications will no longer be sent.
{-# INLINE closeSubscr #-}
{-# DEPRECATED closeSubscr "deprecated in 3.x, use unsubscribe in stead" #-}
closeSubscr :: PtrSubscr -> IO Bool
closeSubscr = runBool libSubscrClose

-- | Releases a reference to the subscription.
-- A count of the references to the subscription is maintained and when this count reaches zero,
-- the memory associated with the subscription is freed.
-- The subscription is also deregistered so that notifications are no longer sent,
-- if this was not already done using the function 'closeSubscr'.
{-# INLINE releaseSubscr #-}
releaseSubscr :: PtrSubscr -> IO Bool
releaseSubscr = runBool libSubscrRelease

-- | Prepares a statement for registration on the subscription.
-- The statement is then registered by calling the function 'prepareStatement'.
-- The reference to the statement that is returned should be released as soon as it is no longer needed.
{-# INLINE subscrPrepareStatement #-}
subscrPrepareStatement
  :: PtrSubscr  -- ^  a reference to the subscription on which the statement is to be prepared for registration.
  -> ByteString -- ^ the SQL that is to be prepared
  -> IO PtrStmt -- ^ a reference to the statement that was prepared
subscrPrepareStatement p sql
  = libSubscrPrepareStmt
    & inCxtPtr p
    & inStrLen sql
    & outCxtPtr p
