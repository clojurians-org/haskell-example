{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.Oracle (connectOracle) where

import Control.Concurrent.MVar(MVar, modifyMVar, modifyMVar_, newMVar, withMVar)
import Control.Exception (evaluate, throw)
import Data.Char(digitToInt, isDigit)
import Data.Maybe(isNothing)
import System.Time(ClockTime(TOD), toClockTime)
import Foreign.C.String(CString, peekCString)
import Foreign.C.Types(CInt)
import Foreign.Ptr(castPtr)
import Foreign.ForeignPtr(withForeignPtr)

import Database.HDBC (IConnection(..), SqlValue)
import Database.HDBC.Statement (Statement(..), SqlValue(..), SqlError(..))
import Database.Oracle.OCIFunctions (EnvHandle, ErrorHandle, ConnHandle,
                                          ServerHandle, SessHandle, StmtHandle,
                                          ColumnInfo, ParamHandle, catchOCI,
                                          envCreate, terminate, defineByPos,
                                          serverAttach, serverDetach,
                                          handleAlloc, handleFree, getParam,
                                          setHandleAttr, getHandleAttr,
                                          setHandleAttrString, getHandleAttrString,
                                          stmtPrepare, stmtExecute, stmtFetch,
                                          sessionBegin, sessionEnd,
                                          descriptorFree, getOCIErrorMsg,
                                          bufferToString, bufferToCaltime,
                                          bufferToByteString)
import Database.Oracle.OCIConstants (oci_HTYPE_ERROR, oci_HTYPE_SERVER,
                                          oci_HTYPE_SVCCTX, oci_HTYPE_SESSION,
                                          oci_HTYPE_TRANS, oci_HTYPE_ENV,
                                          oci_HTYPE_STMT, oci_ATTR_SERVER,
                                          oci_ATTR_USERNAME, oci_ATTR_PASSWORD,
                                          oci_ATTR_SESSION, oci_ATTR_TRANS,
                                          oci_ATTR_PARAM_COUNT, oci_ATTR_NAME,
                                          oci_ATTR_DATA_TYPE,
                                          oci_DTYPE_PARAM, oci_NO_DATA,
                                          oci_CRED_RDBMS,
                                          oci_SQLT_CHR, oci_SQLT_AFC,
                                          oci_SQLT_AVC, oci_SQLT_DAT,
                                          oci_SQLT_NUM, oci_SQLT_FLT,
                                          oci_SQLT_LNG, oci_SQLT_STR,
                                          oci_SQLT_BIN, oci_SQLT_INT,
                                          oci_SQLT_RDD, oci_SQLT_DATE)

data OracleStatement = OracleStatement { state :: StmtState,
                                         handle :: StmtHandle }

data StmtState = Prepared
               | Executed [ColumnDetails]
               | Finished

type ConversionInfo = (CInt, Int, ColumnInfo -> IO SqlValue) -- Output type, Size, Reading function
type ColumnDetails = (IO SqlValue, String) -- read action + columnname

data OracleConnection = OracleConnection EnvHandle ErrorHandle ConnHandle

instance IConnection OracleConnection where
    disconnect = disconnectOracle
    commit _ = fail "Not implemented"
    rollback _ = fail "Not implemented"
    run _ _ _ = fail "Not implemented"
    prepare = prepareOracle
    clone _ = fail "Not implemented"
    hdbcDriverName _ = "oracle"
    hdbcClientVer _ = fail "Not implemented"
    proxiedClientName _ = fail "Not implemented"
    proxiedClientVer _ = fail "Not implemented"
    dbServerVer _ = fail "Not implemented"
    dbTransactionSupport _ = False
    getTables _ = fail "Not implemented"
    describeTable _ _ = fail "Not implemented"

rethrowOCI errorHandle action =
    catchOCI action
             (\exc -> do (code, msg) <- getOCIErrorMsg (castPtr errorHandle) oci_HTYPE_ERROR
                         evaluate $ throw (SqlError "" (fromIntegral code) msg))

connectOracle :: String -> String -> String -> IO OracleConnection
connectOracle user pswd dbname = do
    env <- envCreate
    err <- createHandle oci_HTYPE_ERROR env
    server <- createHandle oci_HTYPE_SERVER env
    serverAttach err server dbname
    conn <- logToServer server user pswd env err
    return (OracleConnection env err conn)

logToServer :: ServerHandle -> String -> String -> EnvHandle -> ErrorHandle -> IO ConnHandle
logToServer srv user pswd env err = do
    conn <- createHandle oci_HTYPE_SVCCTX env
    session <- createHandle  oci_HTYPE_SESSION env
    trans <- createHandle oci_HTYPE_TRANS env
    setHandleAttr err (castPtr conn) oci_HTYPE_SVCCTX (castPtr srv) oci_ATTR_SERVER
    setHandleAttrString err (castPtr session) oci_HTYPE_SESSION user oci_ATTR_USERNAME
    setHandleAttrString err (castPtr session) oci_HTYPE_SESSION pswd oci_ATTR_PASSWORD
    rethrowOCI err $ sessionBegin err conn session oci_CRED_RDBMS
    setHandleAttr err (castPtr conn) oci_HTYPE_SVCCTX (castPtr session) oci_ATTR_SESSION
    setHandleAttr err (castPtr conn) oci_HTYPE_SVCCTX (castPtr trans) oci_ATTR_TRANS
    return conn

disconnectOracle (OracleConnection env err conn) = do
    session <- getHandleAttr err (castPtr conn) oci_HTYPE_SVCCTX oci_ATTR_SESSION
    server <- getHandleAttr err (castPtr conn) oci_HTYPE_SVCCTX oci_ATTR_SERVER
    sessionEnd err conn session
    serverDetach err server
    free session
    free server
    free conn
    free err
    free env

prepareOracle oraconn@(OracleConnection env err conn) query = rethrowOCI err
    (do stmthandle <- createHandle oci_HTYPE_STMT env
        stmtPrepare err stmthandle query
        stmtvar <- newMVar (OracleStatement Prepared stmthandle)
        return (statementFor oraconn stmtvar query))

executeOracle :: OracleConnection -> MVar OracleStatement -> [SqlValue] -> IO Integer
executeOracle (OracleConnection _ err conn) stmtvar bindvars =
    let exec stmt@(OracleStatement (Executed _) _) = return (stmt, 0)
        exec (OracleStatement Prepared stmthandle) = do
            stmtExecute err conn stmthandle 0
            numColumns <- getNumColumns err stmthandle
            convinfos <- flip mapM [1..numColumns] $ \col -> do
                colHandle <- getParam err stmthandle col
                itype <- getHandleAttr err (castPtr colHandle) oci_DTYPE_PARAM oci_ATTR_DATA_TYPE
                colname <- getHandleAttrString err (castPtr colHandle) oci_DTYPE_PARAM oci_ATTR_NAME
                free colHandle
                let Just (otype, size, reader) = search (itype `elem`) dtypeConversion
                colinfo <- defineByPos err stmthandle col size otype
                return (reader colinfo, colname)
            return (OracleStatement (Executed convinfos) stmthandle, 0)
    in rethrowOCI err $ modifyMVar stmtvar exec

getNumColumns err stmt = getHandleAttr err (castPtr stmt) oci_HTYPE_STMT oci_ATTR_PARAM_COUNT

dtypeConversion :: [([CInt], ConversionInfo)]
dtypeConversion = [([oci_SQLT_CHR, oci_SQLT_AFC, oci_SQLT_AVC, oci_SQLT_LNG,
                     oci_SQLT_RDD],
                        (oci_SQLT_STR, 16000, readString)),
                   ([oci_SQLT_DAT, oci_SQLT_DATE], (oci_SQLT_DAT, 7, readTime)),
                   ([oci_SQLT_NUM, oci_SQLT_FLT, oci_SQLT_INT],
                        (oci_SQLT_STR, 40, readNumber)),
                   ([oci_SQLT_BIN], (oci_SQLT_BIN, 2000, readBinary))]

fetchOracleRow :: OracleConnection -> MVar OracleStatement -> IO (Maybe [SqlValue])
fetchOracleRow (OracleConnection _ err _) stmtvar =
    let fetch (OracleStatement Prepared _) = fail "Trying to fetch before executing statement"
        fetch (OracleStatement (Executed convinfos) stmt) = do
            let readCols = map fst convinfos
            fr <- stmtFetch err stmt
            if fr == oci_NO_DATA
             then return Nothing
             else return . Just =<< sequence readCols
    in do result <- withMVar stmtvar fetch
          if isNothing result then finishOracle stmtvar else return ()
          return result

readString = readValue (\(_, buf, nullptr, sizeptr) -> bufferToString (undefined, buf, nullptr, sizeptr))
                       SqlString

readTime = readValue (\(_, buf, nullptr, sizeptr) -> bufferToCaltime nullptr buf)
                     (\time -> let TOD secs _ = toClockTime time in SqlEpochTime secs)

readNumber = readValue (\(_, buf, nullptr, sizeptr) -> bufferToString (undefined, buf, nullptr, sizeptr))
                       strToSqlNum

readBinary = readValue (\(_, buf, nullptr, sizeptr) -> bufferToByteString buf nullptr sizeptr)
                       SqlByteString

readValue ::    (ColumnInfo -> IO (Maybe a))
             -> (a -> SqlValue)
             -> ColumnInfo -> IO SqlValue
readValue read convert colinfo = read colinfo >>= return . maybe SqlNull convert

finishOracle :: MVar OracleStatement -> IO ()
finishOracle = flip modifyMVar_
                    (\stmt -> let stmthandle = handle stmt in
                              do free stmthandle
                                 return $ OracleStatement Finished stmthandle)

getOracleColumnNames :: OracleConnection -> MVar OracleStatement -> IO [String]
getOracleColumnNames (OracleConnection _ err _) stmtvar =
    let getNames (OracleStatement Prepared _) = fail "Trying to read column names before executing"
        getNames (OracleStatement (Executed convinfos) _) = return $ map snd convinfos
    in withMVar stmtvar getNames

createHandle htype env = handleAlloc htype (castPtr env) >>= return.castPtr
disposeHandle htype = handleFree htype . castPtr
disposeDescriptor = descriptorFree oci_DTYPE_PARAM . castPtr

class FreeableHandle h where free :: h -> IO ()

instance FreeableHandle EnvHandle where free = disposeHandle oci_HTYPE_ENV
instance FreeableHandle ErrorHandle where free = disposeHandle oci_HTYPE_ERROR
instance FreeableHandle ServerHandle where free = disposeHandle oci_HTYPE_SERVER
instance FreeableHandle ConnHandle where free = disposeHandle oci_HTYPE_SVCCTX
instance FreeableHandle SessHandle where free = disposeHandle oci_HTYPE_SESSION
instance FreeableHandle StmtHandle where free = disposeHandle oci_HTYPE_STMT
instance FreeableHandle ParamHandle where free = disposeDescriptor

statementFor oraconn stmtvar query =
    Statement {execute = executeOracle oraconn stmtvar,
               executeMany = \_ -> fail "Not implemented",
               finish = finishOracle stmtvar,
               fetchRow = fetchOracleRow oraconn stmtvar,
               originalQuery = query,
               getColumnNames = getOracleColumnNames oraconn stmtvar,
               describeResult = fail "Not implemented"}

search :: (a -> Bool) -> [(a, b)] -> Maybe b
search _ [] = Nothing
search p ((x,y):xys) = if p x then Just y else search p xys

sqlMultiply :: SqlValue -> Integer -> SqlValue
sqlMultiply (SqlDouble x) y = SqlDouble (x * fromInteger y)
sqlMultiply (SqlInteger n) m = SqlInteger (m * n)

strToSqlNum :: String -> SqlValue
strToSqlNum str = sqlMultiply sqlabs sign
    where sqlabs = if any (not . isDigit) strabs then SqlDouble (strToDouble strabs) else SqlInteger (strToInt strabs)
          strabs = dropWhile (== '-') str
          sign = if strabs == str then 1 else -1

strToInt :: String -> Integer
strToInt [] = 0
strToInt (c:cs) = toInteger (digitToInt c) * 10 ^ length cs + strToInt cs

splitBy :: (a -> Bool) -> [a] -> ([a], [a])
splitBy p xs = (takeWhile (not.p) xs, tail (dropWhile (not.p) xs))

strToDouble :: String -> Double
strToDouble str = fromIntegral integral + fraction
    where (istr, fstr) = splitBy (not . isDigit) str
          integral = strToInt istr
          fraction = fromIntegral (strToInt fstr) * 10 ^^ (-(length fstr))
