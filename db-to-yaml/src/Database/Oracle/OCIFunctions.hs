
-- |
-- Module      :  Database.Oracle.OCIFunctions
-- Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
-- License     :  BSD-style
-- Maintainer  :  oleg@pobox.com, alistair@abayley.org
-- Stability   :  experimental
-- Portability :  non-portable

-- Simple wrappers for OCI functions (FFI).

-- The functions in this file are simple wrappers for OCI functions.
-- The wrappers add error detection and exceptions;
-- functions in this module raise 'OCIException'.
-- The next layer up traps these and turns them into 'Database.Enumerator.DBException'.

-- Note that 'OCIException' /does not/ contain the error number and text
-- returned by 'getOCIErrorMsg'.
-- It is the job of the next layer (module) up to catch the 'OCIException'
-- and then call 'getOCIErrorMsg' to get the actual error details.
-- The 'OCIException' simply contains the error number returned by
-- the OCI call, and some text identifying the wrapper function.
-- See 'formatErrorCodeDesc' for the set of possible values for the OCI error numbers.

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Database.Oracle.OCIFunctions where


import Prelude hiding (catch)
import Database.Oracle.OCIConstants
import Database.Oracle.Util
import Foreign
import Foreign.C
import Control.Monad
import Control.Exception
import Data.Dynamic
import Data.Time
import System.Time
import Data.ByteString (pack)

-- |
--  * Each handle type has its own data type, to prevent stupid errors
--    i.e. using the wrong handle at the wrong time.

--  * In GHC you can simply say @data OCIStruct@ i.e. there's no need for @= OCIStruct@.
--    I've decided to be more portable, as it doesn't cost much.

--  * Use castPtr if you need to convert handles (say 'OCIHandle' to a more specific type, or vice versa).

data OCIStruct = OCIStruct
type OCIHandle = Ptr OCIStruct  -- generic Handle for OCI functions that return Handles
data OCIBuffer = OCIBuffer  -- generic buffer. Could hold anything: value or pointer.
type BufferPtr = Ptr OCIBuffer
type BufferFPtr = ForeignPtr OCIBuffer
type ColumnResultBuffer = ForeignPtr OCIBuffer  -- use ForeignPtr to ensure GC'd
-- triple of (nullind, buffer, size)
type BindBuffer = (ForeignPtr CShort, ForeignPtr OCIBuffer, ForeignPtr CUShort)
data Context = Context
type ContextPtr = Ptr Context

data EnvStruct = EnvStruct
type EnvHandle = Ptr EnvStruct
data ErrorStruct = ErrorStruct
type ErrorHandle = Ptr ErrorStruct
data ServerStruct = ServerStruct
type ServerHandle = Ptr ServerStruct
data UserStruct = UserStruct
type UserHandle = Ptr UserStruct
data ConnStruct = ConnStruct
type ConnHandle = Ptr ConnStruct  -- AKA Service Context
data SessStruct = SessStruct
type SessHandle = Ptr SessStruct
data StmtStruct = StmtStruct
type StmtHandle = Ptr StmtStruct
data DefnStruct = DefnStruct
type DefnHandle = Ptr DefnStruct
data ParamStruct = ParamStruct
type ParamHandle = Ptr ParamStruct
data BindStruct = BindStruct
type BindHandle = Ptr BindStruct
type ColumnInfo = (DefnHandle, ColumnResultBuffer, ForeignPtr CShort, ForeignPtr CUShort)


-- |Low-level, OCI library errors.

data OCIException = OCIException CInt String
  deriving (Typeable, Show)

-- If we can't derive Typeable then the following code should do the trick:

--  > data OCIException = OCIException CInt String
--  > ociExceptionTc :: TyCon
--  > ociExceptionTc = mkTyCon "Database.Oracle.OCIFunctions.OCIException"
--  > instance Typeable OCIException where typeOf _ = mkAppTy ociExceptionTc []


catchOCI :: IO a -> (OCIException -> IO a) -> IO a
throwOCI :: OCIException -> a
instance Exception OCIException
catchOCI = catch
throwOCI = throw


mkCInt :: Int -> CInt
mkCInt n = fromIntegral n

mkCShort :: CInt -> CShort
mkCShort n = fromIntegral n

mkCUShort :: CInt -> CUShort
mkCUShort n = fromIntegral n

cStrLen :: CStringLen -> CInt
cStrLen = mkCInt . snd

cStr :: CStringLen -> CString
cStr = fst



-- ---------------------------------------------------------------------------------
-- -- ** Foreign OCI functions
-- ---------------------------------------------------------------------------------


foreign import ccall "OCIEnvCreate" ociEnvCreate :: Ptr EnvHandle -> CInt -> Ptr a -> FunPtr a -> FunPtr a -> FunPtr a -> CInt -> Ptr (Ptr a) -> IO CInt
foreign import ccall "OCIHandleAlloc" ociHandleAlloc :: OCIHandle -> Ptr OCIHandle -> CInt -> CInt -> Ptr a -> IO CInt
foreign import ccall "oci.h OCIHandleFree" ociHandleFree :: OCIHandle -> CInt -> IO CInt
foreign import ccall "oci.h OCIErrorGet" ociErrorGet :: OCIHandle -> CInt -> CString -> Ptr CInt -> CString -> CInt -> CInt -> IO CInt


foreign import ccall "oci.h OCIParamGet" ociParamGet :: OCIHandle -> CInt -> ErrorHandle -> Ptr OCIHandle -> CInt -> IO CInt
foreign import ccall "oci.h OCIDescriptorFree" ociDescriptorFree :: OCIHandle -> CInt -> IO CInt
foreign import ccall "oci.h OCIAttrGet" ociAttrGet
  :: OCIHandle -> CInt -> BufferPtr -> Ptr CInt -> CInt -> ErrorHandle -> IO CInt
foreign import ccall "oci.h OCIAttrSet" ociAttrSet
  :: OCIHandle -> CInt -> BufferPtr -> CInt -> CInt -> ErrorHandle -> IO CInt


foreign import ccall "oci.h OCILogon" ociLogon
  :: EnvHandle -> ErrorHandle -> Ptr ConnHandle -> CString -> CInt -> CString -> CInt -> CString -> CInt -> IO CInt
foreign import ccall "oci.h OCILogoff" ociLogoff :: ConnHandle -> ErrorHandle -> IO CInt
foreign import ccall "oci.h OCISessionBegin" ociSessionBegin :: ConnHandle -> ErrorHandle -> SessHandle -> CInt -> CInt -> IO CInt
foreign import ccall "oci.h OCISessionEnd" ociSessionEnd :: ConnHandle -> ErrorHandle -> SessHandle -> CInt -> IO CInt
foreign import ccall "oci.h OCIServerAttach" ociServerAttach :: ServerHandle -> ErrorHandle -> CString -> CInt -> CInt -> IO CInt
foreign import ccall "oci.h OCIServerDetach" ociServerDetach :: ServerHandle -> ErrorHandle -> CInt -> IO CInt
foreign import ccall "oci.h OCITerminate" ociTerminate :: CInt -> IO CInt

foreign import ccall "oci.h OCITransStart" ociTransStart :: ConnHandle -> ErrorHandle -> Word8 -> CInt -> IO CInt
foreign import ccall "oci.h OCITransCommit" ociTransCommit :: ConnHandle -> ErrorHandle -> CInt -> IO CInt
foreign import ccall "oci.h OCITransRollback" ociTransRollback :: ConnHandle -> ErrorHandle -> CInt -> IO CInt

foreign import ccall "oci.h OCIStmtPrepare" ociStmtPrepare :: StmtHandle -> ErrorHandle -> CString -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall "oci.h OCIDefineByPos" ociDefineByPos
  :: StmtHandle -> Ptr DefnHandle -> ErrorHandle -> CInt -> BufferPtr -> CInt -> CUShort -> Ptr CShort -> Ptr CUShort -> Ptr CUShort -> CInt -> IO CInt
foreign import ccall "oci.h OCIStmtExecute" ociStmtExecute :: ConnHandle -> StmtHandle -> ErrorHandle -> CInt -> CInt -> OCIHandle -> OCIHandle -> CInt -> IO CInt
foreign import ccall "oci.h OCIStmtFetch" ociStmtFetch :: StmtHandle -> ErrorHandle -> CInt -> CShort -> CInt -> IO CInt

-- stmt, ptr bindHdl, err, pos, valuePtr, sizeOfValue,
-- datatype, indicatorPtr, lenArrayPtr, retCodeArrayPtr,
-- plsqlArrayMaxLen, plsqlCurrEltPtr, mode

foreign import ccall "oci.h OCIBindByPos" ociBindByPos ::
  StmtHandle
  -> Ptr BindHandle
  -> ErrorHandle
  -> CUInt  -- ^ position
  -> BufferPtr  -- ^ buffer containing data
  -> CInt  -- ^ max size of buffer
  -> CUShort  -- ^ SQL data type
  -> Ptr CShort  -- ^ null indicator ptr
  -> Ptr CUShort  -- ^ input + output size, or array of sizes
  -> Ptr CUShort  -- ^ array of return codes
  -> CUInt  -- ^ max array elements
  -> Ptr CUInt  -- ^ number of array elements
  -> CUInt  -- ^ mode
  -> IO CInt

foreign import ccall "oci.h OCIBindDynamic" ociBindDynamic ::
  BindHandle -> ErrorHandle -> ContextPtr -> FunPtr OCICallbackInBind
  -> ContextPtr -> FunPtr OCICallbackOutBind -> IO CInt

type OCICallbackInBind = ContextPtr -> BindHandle -> CInt -> CInt
  -> Ptr BufferPtr -> CInt -> Ptr Word8 -> Ptr CShort -> IO CInt

type OCICallbackOutBind = ContextPtr -> BindHandle -> CInt -> CInt
  -> Ptr BufferPtr -> Ptr CInt -> Ptr Word8 -> Ptr CShort -> Ptr (Ptr CShort) -> IO CInt

foreign import ccall "wrapper" mkOCICallbackInBind ::
  OCICallbackInBind -> IO (FunPtr OCICallbackInBind)
foreign import ccall "wrapper" mkOCICallbackOutBind ::
  OCICallbackOutBind -> IO (FunPtr OCICallbackOutBind)

-- ---------------------------------------------------------------------------------
-- -- ** OCI error reporting
-- ---------------------------------------------------------------------------------

-- |This is just an auxiliary function for getOCIErrorMsg.

getOCIErrorMsg2 :: OCIHandle -> CInt -> Ptr CInt -> CString -> CInt -> IO (CInt, String)
getOCIErrorMsg2 ocihandle handleType errCodePtr errMsgBuf maxErrMsgLen = do
  rc <- ociErrorGet ocihandle 1 nullPtr errCodePtr errMsgBuf maxErrMsgLen handleType
  if rc < 0
    then return (0, "Error message not available.")
    else do
      msg <- peekCString errMsgBuf
      e <- peek errCodePtr
      return (e, msg)


getOCIErrorMsg :: OCIHandle -> CInt -> IO (CInt, String)
getOCIErrorMsg ocihandle handleType = do
  let stringBufferLen = 1000
  allocaBytes stringBufferLen $ \errMsg ->
    alloca $ \errCode ->
    getOCIErrorMsg2 ocihandle handleType errCode errMsg (mkCInt stringBufferLen)

fromEnumOCIErrorCode :: CInt -> String
fromEnumOCIErrorCode err
  | err == oci_SUCCESS = "OCI_SUCCESS"
  | err == oci_SUCCESS_WITH_INFO = "OCI_SUCCESS_WITH_INFO"
  | err == oci_NEED_DATA = "OCI_NEED_DATA"
  | err == oci_NO_DATA = "OCI_NO_DATA"
  | err == oci_INVALID_HANDLE = "OCI_INVALID_HANDLE"
  | err == oci_STILL_EXECUTING = "OCI_STILL_EXECUTING"
  | err == oci_CONTINUE = "OCI_CONTINUE"
  | err == oci_RESERVED_FOR_INT_USE = "OCI_RESERVED_FOR_INT_USE"
  | otherwise = "OCI_ERROR"

formatErrorCodeDesc :: CInt -> String -> String
formatErrorCodeDesc err desc
  | err == oci_ERROR = ""
  | otherwise = (fromEnumOCIErrorCode err) ++ " - " ++ desc


-- |Given the two parts of an 'OCIException' (the error number and text)
-- get the actual error message from the DBMS and construct an error message
-- from all of these pieces.

formatOCIMsg :: CInt -> String -> OCIHandle -> CInt -> IO (Int, String)
formatOCIMsg e m ocihandle handleType = do
  (err, msg) <- getOCIErrorMsg ocihandle handleType
  --return (fromIntegral err, (formatErrorCodeDesc e m) ++ " : " ++ (show err) ++ " - " ++ msg)
  if msg == ""
    then return (fromIntegral err, (formatErrorCodeDesc e m))
    else return (fromIntegral err, (formatErrorCodeDesc e m) ++ " : " ++ msg)



-- |We have two format functions: 'formatEnvMsg' takes the 'EnvHandle',
-- 'formatErrorMsg' takes the 'ErrorHandle'.
-- They're just type-safe wrappers for 'formatMsgCommon'.

formatMsgCommon :: OCIException -> OCIHandle -> CInt -> IO (Int, String)
formatMsgCommon (OCIException e m) h handleType = do
  if e == 0
    then return (0, "")
    else case () of
      _ | e == oci_ERROR -> do (formatOCIMsg e m h handleType)
        | e == oci_SUCCESS_WITH_INFO -> do (formatOCIMsg e m h handleType)
        | otherwise -> return (fromIntegral e, formatErrorCodeDesc e m)

formatErrorMsg :: OCIException -> ErrorHandle -> IO (Int, String)
formatErrorMsg exc err = formatMsgCommon exc (castPtr err) oci_HTYPE_ERROR

formatEnvMsg :: OCIException -> EnvHandle -> IO (Int, String)
formatEnvMsg exc err = formatMsgCommon exc (castPtr err) oci_HTYPE_ENV



-- |The testForError functions are the only places where OCIException is thrown,
-- so if you want to change or embellish it, your changes will be localised here.
-- These functions factor out common error handling code
-- from the OCI wrapper functions that follow.

-- Typically an OCI wrapper function would look like:

--  > handleAlloc handleType env = alloca ptr -> do
--  >   rc <- ociHandleAlloc env ptr handleType 0 nullPtr
--  >   if rc < 0
--  >     then throwOCI (OCIException rc msg)
--  >     else return ()

-- where the code from @if rc < 0@ onwards was identical.
-- 'testForError' replaces the code from @if rc < 0 ...@ onwards.

testForError :: CInt -> String -> a -> IO a
testForError rc msg retval = do
  if rc < 0
    then throwOCI (OCIException rc msg)
    else return retval


-- |Like 'testForError' but when the value you want to return
-- is at the end of a pointer.
-- Either there was an error, in which case the pointer probably isn't valid,
-- or there is something at the end of the pointer to return.
-- See 'dbLogon' and 'getHandleAttr' for example usage.

testForErrorWithPtr :: Storable a => CInt -> String -> Ptr a -> IO a
testForErrorWithPtr rc msg retval = do
  if rc < 0
    then throwOCI (OCIException rc msg)
    else peek retval

testForErrorWithString :: CInt -> String -> Ptr (Ptr CChar) -> Ptr Int -> IO String
testForErrorWithString rc msg retval sizePtr = do
  if rc < 0
    then throwOCI (OCIException rc msg)
    else do size <- peek sizePtr
            str  <- peek retval
            peekCStringLen (str, size)

-- ---------------------------------------------------------------------------------
-- -- ** Allocating Handles (i.e. creating OCI data structures, and memory management)
-- ---------------------------------------------------------------------------------


envCreate :: IO EnvHandle
envCreate = alloca $ \ptr -> do
  rc <- ociEnvCreate ptr oci_THREADED nullPtr nullFunPtr nullFunPtr nullFunPtr 0 nullPtr
  testForErrorWithPtr rc "allocate initial end" ptr

handleAlloc :: CInt -> OCIHandle -> IO OCIHandle
handleAlloc handleType env = alloca $ \ptr -> do
  rc <- ociHandleAlloc env ptr handleType 0 nullPtr
  testForErrorWithPtr rc "allocate handle" ptr

handleFree :: CInt -> OCIHandle -> IO ()
handleFree handleType ptr = do
   rc <- ociHandleFree ptr handleType
   testForError rc "free handle" ()



setHandleAttr :: ErrorHandle -> OCIHandle -> CInt -> Ptr a -> CInt -> IO ()
setHandleAttr err ocihandle handleType handleAttr attrType = do
  rc <- ociAttrSet ocihandle handleType (castPtr handleAttr) 0 attrType err
  testForError rc "setHandleAttr" ()


setHandleAttrString :: ErrorHandle -> OCIHandle -> CInt -> String -> CInt -> IO ()
setHandleAttrString err ocihandle handleType s attrType = do
  withCStringLen s $ \sC -> do
    rc <- ociAttrSet ocihandle handleType (castPtr (cStr sC)) (cStrLen sC) attrType err
    testForError rc "setHandleAttrString" ()


-- ociAttrGet returns a pointer to something - maybe a handle or a chunk of memory.
-- Sometimes it's a pointer to a Handle, i.e. a Ptr to a Ptr to a Struct,
-- so we want to peek it to get the Handle.
-- Other times it's a pointer to (say) a few bytes which might contain a number or a string.
-- Deref'ing it returns that value immediately, rather than a Ptr to that value.

getHandleAttr :: (Storable a) => ErrorHandle -> OCIHandle -> CInt -> CInt -> IO a
getHandleAttr err ocihandle handleType attrType = alloca $ \ptr -> do
  -- 3rd arg has type Ptr OCIBuffer.
  rc <- ociAttrGet ocihandle handleType (castPtr ptr) nullPtr attrType err
  testForErrorWithPtr rc "getAttrHandle" ptr
  
getHandleAttrString :: ErrorHandle -> OCIHandle -> CInt -> CInt -> IO String
getHandleAttrString err ocihandle handleType attrType = alloca $ \ptr -> alloca $ \sizePtr -> do
  -- 3rd arg has type Ptr OCIBuffer.
  rc <- ociAttrGet ocihandle handleType (castPtr ptr) (castPtr sizePtr) attrType err
  testForErrorWithString rc "getHandleAttrString" ptr sizePtr

getParam :: ErrorHandle -> StmtHandle -> Int -> IO ParamHandle
getParam err stmt posn = alloca $ \ptr -> do
  rc <- ociParamGet (castPtr stmt) oci_HTYPE_STMT err ptr (mkCInt posn)
  testForErrorWithPtr rc "getParam" (castPtr ptr)

descriptorFree :: CInt -> OCIHandle -> IO ()
descriptorFree descriptorType ptr = do
   rc <- ociDescriptorFree ptr descriptorType
   testForError rc "free frscriptor" ()


-- ---------------------------------------------------------------------------------
-- -- ** Connecting and detaching
-- ---------------------------------------------------------------------------------

-- |The OCI Logon function doesn't behave as you'd expect when the password is due to expire.
-- 'ociLogon' returns 'Database.Oracle.OCIConstants.oci_SUCCESS_WITH_INFO',
-- but the 'ConnHandle' returned is not valid.
-- In this case we have to change 'Database.Oracle.OCIConstants.oci_SUCCESS_WITH_INFO'
-- to 'Database.Oracle.OCIConstants.oci_ERROR',
-- so that the error handling code will catch it and abort. 
-- I don't know why the handle returned isn't valid,
-- as the logon process should be able to complete successfully in this case.


dbLogon :: String -> String -> String -> EnvHandle -> ErrorHandle -> IO ConnHandle
dbLogon user pswd db env err =
  withCStringLen user $ \userC ->
  withCStringLen pswd $ \pswdC ->
  withCStringLen db   $ \dbC ->
  alloca $ \conn -> do
    rc <- ociLogon env err conn (cStr userC) (cStrLen userC) (cStr pswdC) (cStrLen pswdC) (cStr dbC) (cStrLen dbC)
    case () of
      _ | rc == oci_SUCCESS_WITH_INFO -> testForErrorWithPtr oci_ERROR "logon" conn
        | otherwise -> testForErrorWithPtr rc "logon" conn


dbLogoff :: ErrorHandle -> ConnHandle -> IO ()
dbLogoff err conn = do
  rc <- ociLogoff conn err
  testForError rc "logoff" ()


terminate :: IO ()
terminate = do
  rc <- ociTerminate oci_DEFAULT
  testForError rc "terminate" ()



serverDetach :: ErrorHandle -> ServerHandle -> IO ()
serverDetach err server = do
  rc <- ociServerDetach server err oci_DEFAULT
  testForError rc "server detach" ()


serverAttach :: ErrorHandle -> ServerHandle -> String -> IO ()
serverAttach err server dblink = do
  withCStringLen dblink $ \s -> do
    rc <- ociServerAttach server err (cStr s) (cStrLen s) oci_DEFAULT
    testForError rc "server attach" ()


-- |Having established a connection (Service Context), now get the Session.
-- You can have more than one session per connection,
-- but I haven't implemented it yet.

getSession :: ErrorHandle -> ConnHandle -> IO SessHandle
getSession err conn = liftM castPtr (getHandleAttr err (castPtr conn) oci_HTYPE_SVCCTX oci_ATTR_SESSION)


sessionBegin :: ErrorHandle -> ConnHandle -> SessHandle -> CInt -> IO ()
sessionBegin err conn sess cred = do
  rc <- ociSessionBegin conn err sess cred oci_DEFAULT
  testForError rc "session begin" ()


sessionEnd :: ErrorHandle -> ConnHandle -> SessHandle -> IO ()
sessionEnd err conn sess = do
  rc <- ociSessionEnd conn err sess oci_DEFAULT
  testForError rc "session end" ()



-- ---------------------------------------------------------------------------------
-- -- ** Transactions
-- ---------------------------------------------------------------------------------

beginTrans :: ErrorHandle -> ConnHandle -> CInt -> IO ()
beginTrans err conn isolation = do
  rc <- ociTransStart conn err 0 isolation
  testForError rc "begin transaction" ()

commitTrans :: ErrorHandle -> ConnHandle -> IO ()
commitTrans err conn = do
  rc <- ociTransCommit conn err oci_DEFAULT
  testForError rc "commit" ()

rollbackTrans :: ErrorHandle -> ConnHandle -> IO ()
rollbackTrans err conn = do
  rc <- ociTransRollback conn err oci_DEFAULT
  testForError rc "rollback" ()


-- ---------------------------------------------------------------------------------
-- -- ** Issuing queries
-- ---------------------------------------------------------------------------------

-- |With the OCI you do queries with these steps:

--  * prepare your statement (it's just a String) - no communication with DBMS

--  * execute it (this sends it to the DBMS for parsing etc)

--  * allocate result set buffers by calling 'defineByPos' for each column

--  * call fetch for each row.

--  * call 'handleFree' for the 'StmtHandle'
--    (I assume this is the approved way of terminating the query;
--    the OCI docs aren't explicit about this.)


stmtPrepare :: ErrorHandle -> StmtHandle -> String -> IO ()
stmtPrepare err stmt sqltext = do
  withCStringLen sqltext $ \sqltextC -> do
    rc <- ociStmtPrepare stmt err (cStr sqltextC) (cStrLen sqltextC) oci_NTV_SYNTAX oci_DEFAULT
    testForError rc "stmtPrepare" ()


stmtExecute :: ErrorHandle -> ConnHandle -> StmtHandle -> Int -> IO ()
stmtExecute err conn stmt iterations = do
  rc <- ociStmtExecute conn stmt err (mkCInt iterations) 0 nullPtr nullPtr oci_DEFAULT
  testForError rc "stmtExecute" ()



-- |defineByPos allocates memory for a single column value.
-- The allocated components are:

--  * the result (i.e. value) - you have to say how big with bufsize.

--  * the null indicator (int16)

--  * the size of the returned data (int16)

-- Previously it was the caller's responsibility to free the memory after they're done with it.
-- Now we use 'Foreign.ForeignPtr.mallocForeignPtr', so manual memory management is hopefully
-- a thing of the past.
-- The caller will also have to cast the data in bufferptr to the expected type
-- (using 'Foreign.Ptr.castPtr').


defineByPos :: ErrorHandle
  -> StmtHandle
  -> Int   -- ^ Position
  -> Int   -- ^ Buffer size in bytes
  -> CInt  -- ^ SQL Datatype (from "Database.Oracle.OCIConstants")
  -> IO ColumnInfo  -- ^ tuple: (DefnHandle, Ptr to buffer, Ptr to null indicator, Ptr to size of value in buffer)
defineByPos err stmt posn bufsize sqldatatype = do
  bufferFPtr <- mallocForeignPtrBytes bufsize
  nullIndFPtr <- mallocForeignPtr
  retSizeFPtr <- mallocForeignPtr
  alloca $ \defnPtr ->
    withForeignPtr bufferFPtr $ \bufferPtr ->
    withForeignPtr nullIndFPtr $ \nullIndPtr ->
    withForeignPtr retSizeFPtr $ \retSizePtr -> do
    rc <- ociDefineByPos stmt defnPtr err (mkCInt posn) bufferPtr (mkCInt bufsize) (mkCUShort sqldatatype) nullIndPtr retSizePtr nullPtr oci_DEFAULT
    defn <- peek defnPtr  -- no need for caller to free defn; I think freeing the stmt handle does it.
    testForError rc "defineByPos" (defn, bufferFPtr, nullIndFPtr, retSizeFPtr)


-- |Oracle only understands bind variable placeholders using syntax :x,
-- where x is a number or a variable name.
-- Most other DBMS's use ? as a placeholder,
-- so we have this function to substitute ? with :n,
-- where n starts at one and increases with each ?.

-- We don't use this function in this library though;
-- it's used in the higher-level implementation of Enumerator.
-- We prefer to retain flexibility at this lower-level,
-- and not force arbitrary implementation choices too soon.
-- If you want to use this library and use :x style syntax, you can.

substituteBindPlaceHolders sql =
  sbph sql 1 False ""

sbph :: String -> Int -> Bool -> String -> String
sbph [] _ _ acc = reverse acc
sbph ('\'':cs) i inQuote acc = sbph cs i (not inQuote) ('\'':acc)
sbph ('?':cs) i False acc = sbph cs (i+1) False ((reverse (show i)) ++ (':':acc))
sbph (c:cs) i inQuote acc = sbph cs i inQuote (c:acc)



bindByPos ::
  ErrorHandle
  -> StmtHandle
  -> Int   -- ^ Position
  -> CShort   -- ^ Null ind: 0 == not null, -1 == null
  -> BufferPtr  -- ^ payload
  -> Int   -- ^ payload size in bytes
  -> CInt  -- ^ SQL Datatype (from "Database.Oracle.OCIConstants")
  -> IO ()
bindByPos err stmt pos nullInd bufptr sze sqltype = do
  indFPtr <- mallocForeignPtr
  sizeFPtr <- mallocForeignPtr
  withForeignPtr indFPtr $ \p -> poke p nullInd
  -- You can't put any old junk in the return-size field,
  -- even if the parameter is IN-only.
  -- So tell it how big the input buffer is.
  withForeignPtr sizeFPtr $ \p -> poke p (fromIntegral sze)
  bufFPtr <- newForeignPtr_ bufptr
  bindOutputByPos err stmt pos (indFPtr, bufFPtr, sizeFPtr) sze sqltype
  return ()

-- Note that this function takes a ForeignPtr to the output-size
-- (in the triple) and also a size parameter, which is the input size.
-- We need to provide both, apparently, regardless of actual parameter
-- direction.

bindOutputByPos ::
  ErrorHandle
  -> StmtHandle
  -> Int   -- ^ Position
  -> BindBuffer  -- ^ triple of (null-ind, payload, input-size)
  -> Int   -- ^ buffer max size in bytes
  -> CInt  -- ^ SQL Datatype (from "Database.Oracle.OCIConstants")
  -> IO BindHandle
bindOutputByPos err stmt pos (nullIndFPtr, bufFPtr, sizeFPtr) sze sqltype =
  alloca $ \bindHdl ->
    withForeignPtr nullIndFPtr $ \indPtr -> do
      withForeignPtr sizeFPtr $ \sizePtr ->
        withForeignPtr bufFPtr $ \bufPtr -> do
          rc <- ociBindByPos stmt bindHdl err (fromIntegral pos) bufPtr
              (fromIntegral sze) (fromIntegral sqltype)
              indPtr sizePtr nullPtr 0 nullPtr (fromIntegral oci_DEFAULT)
          testForError rc "bindOutputByPos" ()
          bptr <- peek bindHdl
          return bptr


-- | Fetch a single row into the buffers.
-- If you have specified a prefetch count > 1 then the row
-- might already be cached by the OCI library.

stmtFetch :: ErrorHandle -> StmtHandle -> IO CInt
stmtFetch err stmt = do
  let numRowsToFetch = 1
  rc <- ociStmtFetch stmt err numRowsToFetch (mkCShort oci_FETCH_NEXT) oci_DEFAULT
  if rc == oci_NO_DATA
    then return rc
    else testForError rc "stmtFetch" rc

-- From the "Bindind and Defining" chapter in the OCI docs:

-- Binding RETURNING...INTO variables

-- An OCI application implements the placeholders in the RETURNING clause
-- as pure OUT bind variables. However, all binds in the RETURNING clause
-- are initially IN and must be properly initialized.
-- To provide a valid value, you can provide a NULL indicator
-- and set that indicator to -1 (NULL).

-- An application must adhere to the following rules when working with
-- bind variables in a RETURNING clause:

--    1. Bind RETURNING clause placeholders in OCI_DATA_AT_EXEC mode using
--       OCIBindByName() or OCIBindByPos(), followed by a call to
--       OCIBindDynamic() for each placeholder.

--       Note: The OCI only supports the callback mechanism for
--       RETURNING clause binds. The polling mechanism is not supported.

--    2. When binding RETURNING clause placeholders, you must supply a valid out
--       bind function as the ocbfp parameter of the OCIBindDynamic() call.
--       This function must provide storage to hold the returned data.

--    3. The icbfp parameter of OCIBindDynamic() call should provide a
--       "dummy" function which returns NULL values when called.

--    4. The piecep parameter of OCIBindDynamic() must be set to OCI_ONE_PIECE.

--    5. No duplicate binds are allowed in a DML statement with a
--       RETURNING clause, such as no duplication between bind variables
--       in the DML section and the RETURNING section of the statement.

-- |Short-circuit null test: if the buffer contains a null then return Nothing.
-- Otherwise, run the IO action to extract a value from the buffer and return Just it.

maybeBufferNull :: ForeignPtr CShort -> Maybe a -> IO a -> IO (Maybe a)
maybeBufferNull nullIndFPtr nullVal action =
  withForeignPtr nullIndFPtr $ \nullIndPtr -> do
    nullInd <- liftM cShort2Int (peek nullIndPtr)
    if (nullInd == -1)  -- -1 == null, 0 == value
      then return nullVal
      else do
        v <- action
        return (Just v)


nullByte :: CChar
nullByte = 0

cShort2Int :: CShort -> Int
cShort2Int n = fromIntegral n

cUShort2Int :: CUShort -> Int
cUShort2Int n = fromIntegral n

cuCharToInt :: CUChar -> Int
cuCharToInt c = fromIntegral c

byteToInt :: Ptr CUChar -> Int -> IO Int
byteToInt buffer n = do
  b <- peekByteOff buffer n
  return (cuCharToInt b)



bufferToString :: ColumnInfo -> IO (Maybe String)
bufferToString (_, bufFPtr, nullFPtr, sizeFPtr) =
  withForeignPtr nullFPtr $ \nullIndPtr -> do
    nullInd <- liftM cShort2Int (peek nullIndPtr)
    if (nullInd == -1)  -- -1 == null, 0 == value
      then return Nothing
      else do
        -- Given a column buffer, extract a string of variable length
        -- (you have to terminate it yourself).
        withForeignPtr bufFPtr $ \bufferPtr ->
          withForeignPtr sizeFPtr $ \retSizePtr -> do
            retsize <- liftM cUShort2Int (peek retSizePtr)
            --putStrLn ("bufferToString: size = " ++ show retsize)
            pokeByteOff (castPtr bufferPtr) retsize nullByte
            val <- peekCString (castPtr bufferPtr)
            return (Just val)

bufferToByteString bufFPtr nullFPtr sizeFPtr =
  withForeignPtr nullFPtr $ \nullIndPtr -> do
    nullInd <- liftM cShort2Int (peek nullIndPtr)
    if (nullInd == -1)  -- -1 == null
      then return Nothing
      else do
        withForeignPtr bufFPtr $ \bufferPtr ->
          withForeignPtr sizeFPtr $ \retSizePtr -> do
            retsize <- liftM cUShort2Int (peek retSizePtr)
            return . Just . pack =<< peekArray retsize (castPtr bufferPtr)

-- | Oracle's excess-something-or-other encoding for years:
-- year = 100*(c - 100) + (y - 100),
-- c = (year div 100) + 100,
-- y = (year mod 100) + 100.

-- +1999 -> 119, 199
-- +0100 -> 101, 100
-- +0001 -> 100, 101
-- -0001 -> 100,  99
-- -0100 ->  99, 100
-- -1999 ->  81,   1

makeYear :: Int -> Int -> Int
makeYear c100 y100 = 100 * (c100 - 100) + (y100 - 100)

makeYearByte :: Int -> Word8
makeYearByte y = fromIntegral ((rem y 100) + 100)

makeCentByte :: Int -> Word8
makeCentByte y = fromIntegral ((quot y 100) + 100)

dumpBuffer :: Ptr Word8 -> IO ()
dumpBuffer buf = do
  dumpByte 0
  dumpByte 1
  dumpByte 2
  dumpByte 3
  dumpByte 4
  dumpByte 5
  dumpByte 6
  putStrLn ""
  where
  dumpByte n = do
    b <- (peekByteOff buf n :: IO Word8)
    putStr $ (show b) ++ " "


bufferToCaltime :: ForeignPtr CShort -> BufferFPtr -> IO (Maybe CalendarTime)
bufferToCaltime nullind fptr = maybeBufferNull nullind Nothing $
  withForeignPtr fptr $ \bufferPtr -> do
    let buffer = castPtr bufferPtr
    --dumpBuffer (castPtr buffer)
    century100 <- byteToInt buffer 0
    year100 <- byteToInt buffer 1
    month <- byteToInt buffer 2
    day <- byteToInt buffer 3
    hour <- byteToInt buffer 4
    minute <- byteToInt buffer 5
    second <- byteToInt buffer 6
    return $ CalendarTime
      { ctYear = makeYear century100 year100
      , ctMonth = toEnum (month - 1)
      , ctDay = day
      , ctHour = hour - 1
      , ctMin = minute - 1
      , ctSec = second - 1
      , ctPicosec = 0
      , ctWDay = Sunday
      , ctYDay = -1
      , ctTZName = "UTC"
      , ctTZ = 0
      , ctIsDST = False
      }

bufferToUTCTime :: ForeignPtr CShort -> BufferFPtr -> IO (Maybe UTCTime)
bufferToUTCTime nullind fptr = maybeBufferNull nullind Nothing $
  withForeignPtr fptr $ \bufferPtr -> do
    let buffer = castPtr bufferPtr
    --dumpBuffer (castPtr buffer)
    century100 <- byteToInt buffer 0
    year100 <- byteToInt buffer 1
    month <- byteToInt buffer 2
    day <- byteToInt buffer 3
    hour <- byteToInt buffer 4
    minute <- byteToInt buffer 5
    second <- byteToInt buffer 6
    let year = makeYear century100 year100
    return (mkUTCTime year month day (hour-1) (minute-1) (second-1))

setBufferByte :: BufferPtr -> Int -> Word8 -> IO ()
setBufferByte buf n v = pokeByteOff buf n v

calTimeToBuffer :: BufferPtr -> CalendarTime -> IO ()
calTimeToBuffer buf ct = do
  setBufferByte buf 0 (makeCentByte (ctYear ct))
  setBufferByte buf 1 (makeYearByte (ctYear ct))
  setBufferByte buf 2 (fromIntegral ((fromEnum (ctMonth ct)) + 1))
  setBufferByte buf 3 (fromIntegral (ctDay ct))
  setBufferByte buf 4 (fromIntegral (ctHour ct + 1))
  setBufferByte buf 5 (fromIntegral (ctMin ct + 1))
  setBufferByte buf 6 (fromIntegral (ctSec ct + 1))

utcTimeToBuffer :: BufferPtr -> UTCTime -> IO ()
utcTimeToBuffer buf utc = do
  let (LocalTime ltday time) = utcToLocalTime (hoursToTimeZone 0) utc
  let (TimeOfDay hour minute second) = time
  let (year, month, day) = toGregorian ltday
  setBufferByte buf 0 (makeCentByte (fromIntegral year))
  setBufferByte buf 1 (makeYearByte (fromIntegral year))
  setBufferByte buf 2 (fromIntegral month)
  setBufferByte buf 3 (fromIntegral day)
  setBufferByte buf 4 (fromIntegral (hour+1))
  setBufferByte buf 5 (fromIntegral (minute+1))
  setBufferByte buf 6 (round (second+1))


bufferPeekValue :: (Storable a) => BufferFPtr -> IO a
bufferPeekValue buffer = do
  v <- withForeignPtr buffer $ \bufferPtr -> peek $ castPtr bufferPtr
  return v

bufferToA :: (Storable a) => ForeignPtr CShort -> BufferFPtr -> IO (Maybe a)
bufferToA nullind buffer = maybeBufferNull nullind Nothing (bufferPeekValue buffer)

bufferToCInt :: ForeignPtr CShort -> BufferFPtr -> IO (Maybe CInt)
bufferToCInt = bufferToA

bufferToInt :: ForeignPtr CShort -> BufferFPtr -> IO (Maybe Int)
bufferToInt nullind b = do
  cint <- bufferToCInt nullind b
  return $ maybe Nothing (Just . fromIntegral) cint

bufferToCDouble :: ForeignPtr CShort -> BufferFPtr -> IO (Maybe CDouble)
bufferToCDouble = bufferToA

bufferToDouble :: ForeignPtr CShort -> BufferFPtr -> IO (Maybe Double)
bufferToDouble nullind b = do
  cdbl <- bufferToCDouble nullind b
  return $ maybe Nothing (Just . realToFrac) cdbl

bufferToStmtHandle :: BufferFPtr -> IO StmtHandle
bufferToStmtHandle buffer = do
  withForeignPtr buffer $ \bufferPtr -> do
    v <- peek (castPtr bufferPtr)
    return v
