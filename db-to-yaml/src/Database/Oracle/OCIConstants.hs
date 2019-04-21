
-- |
-- Module      :  Database.Oracle.OCIConstants
-- Copyright   :  (c) 2004 Oleg Kiselyov, Alistair Bayley
-- License     :  BSD-style
-- Maintainer  :  oleg@pobox.com, alistair@abayley.org
-- Stability   :  experimental
-- Portability :  non-portable

-- Contains CInt equivalents of the #defines in the oci library headers.
-- This is not a complete set; just enough to get the Haskell libraries working.
-- This also might not be particularly portable, but I don't think Oracle are going
-- to change these in a hurry (that would break compiled programs, wouldn't it?).


module Database.Oracle.OCIConstants where

import Foreign.C.Types


-- ** Used all over the place:

oci_DEFAULT :: CInt
oci_DEFAULT = 0

oci_THREADED :: CInt
oci_THREADED = 1

-- ** Handle types:

-- | Found in $ORAHOME\/oci\/include\/oci.h

[ oci_HTYPE_ENV
  , oci_HTYPE_ERROR
  , oci_HTYPE_SVCCTX
  , oci_HTYPE_STMT
  , oci_HTYPE_BIND
  , oci_HTYPE_DEFINE
  , oci_HTYPE_DESCRIBE
  , oci_HTYPE_SERVER
  , oci_HTYPE_SESSION
  , oci_HTYPE_TRANS
  ] = [1..10] :: [CInt]

oci_DTYPE_PARAM :: CInt
oci_DTYPE_PARAM = 53

-- ** Error code types:

-- | Found in $ORAHOME\/oci\/include\/oci.h

[ oci_SUCCESS
  , oci_SUCCESS_WITH_INFO
  , oci_RESERVED_FOR_INT_USE
  , oci_NO_DATA
  , oci_ERROR
  , oci_INVALID_HANDLE
  , oci_NEED_DATA
  , oci_STILL_EXECUTING
  , oci_CONTINUE
  ] =
  [0, 1, 200, 100, -1, -2, 99, -3123, -24200] :: [CInt]



-- ** Attribute types:

-- | Found in $ORAHOME\/oci\/include\/oci.h

[ oci_ATTR_DATA_TYPE
  , oci_ATTR_NAME
  , oci_ATTR_ENV
  , oci_ATTR_SERVER
  , oci_ATTR_SESSION
  , oci_ATTR_TRANS
  , oci_ATTR_ROW_COUNT
  , oci_ATTR_PREFETCH_ROWS
  , oci_ATTR_PARAM_COUNT
  , oci_ATTR_USERNAME
  , oci_ATTR_PASSWORD
  ] = [2,4,5,6,7,8,9,11,18,22,23] :: [CInt]

-- ** Authentication options:

-- | Found in $ORAHOME\/oci\/include\/oci.h

oci_CRED_RDBMS :: CInt
oci_CRED_RDBMS = 1
oci_CRED_EXT :: CInt
oci_CRED_EXT = 2
oci_CRED_PROXY :: CInt
oci_CRED_PROXY = 3



-- ** Syntax types (i.e. does the DBMS understand v7 or v8 syntax, etc):

-- | Found in $ORAHOME\/oci\/include\/oci.h

oci_NTV_SYNTAX :: CInt
oci_NTV_SYNTAX = 1



-- ** Scrollable Cursor Options:

-- | Found in $ORAHOME\/oci\/include\/oci.h

oci_FETCH_NEXT :: CInt
oci_FETCH_NEXT = 2
oci_FETCH_FIRST :: CInt
oci_FETCH_FIRST = 4
oci_FETCH_LAST :: CInt
oci_FETCH_LAST = 8
oci_FETCH_PRIOR :: CInt
oci_FETCH_PRIOR = 16
oci_FETCH_ABSOLUTE :: CInt
oci_FETCH_ABSOLUTE = 32
oci_FETCH_RELATIVE :: CInt
oci_FETCH_RELATIVE = 64
oci_FETCH_RESERVED :: CInt
oci_FETCH_RESERVED = 128




-- ** OCI datatypes:

-- | Found in $ORAHOME\/oci\/include\/ocidfn.h

oci_SQLT_CHR :: CInt
oci_SQLT_CHR = 1
oci_SQLT_NUM :: CInt
oci_SQLT_NUM = 2
oci_SQLT_INT :: CInt
oci_SQLT_INT = 3
oci_SQLT_FLT :: CInt
oci_SQLT_FLT = 4
oci_SQLT_STR :: CInt
oci_SQLT_STR = 5
oci_SQLT_VNU :: CInt
oci_SQLT_VNU = 6
oci_SQLT_LNG :: CInt
oci_SQLT_LNG = 8
oci_SQLT_VCS :: CInt
oci_SQLT_VCS = 9
oci_SQLT_RID :: CInt
oci_SQLT_RID = 11
oci_SQLT_DAT :: CInt
oci_SQLT_DAT = 12
oci_SQLT_VBI :: CInt
oci_SQLT_VBI = 15
oci_SQLT_BIN :: CInt
oci_SQLT_BIN = 23
oci_SQLT_LBI :: CInt
oci_SQLT_LBI = 24
oci_SQLT_UIN :: CInt
oci_SQLT_UIN = 68
oci_SQLT_LVC :: CInt
oci_SQLT_LVC = 94
oci_SQLT_LVB :: CInt
oci_SQLT_LVB = 95
oci_SQLT_AFC :: CInt
oci_SQLT_AFC = 96
oci_SQLT_AVC :: CInt
oci_SQLT_AVC = 97
oci_SQLT_RDD :: CInt
oci_SQLT_RDD = 104
oci_SQLT_RSET :: CInt
oci_SQLT_RSET = 116
oci_SQLT_DATE :: CInt
oci_SQLT_DATE = 184




-- ** Transaction types; parameters for ociTransStart.

-- | Found in $ORAHOME\/oci\/include\/oci.h.
-- There are more than this, but they're related to complicated
-- transaction-management stuff in the OCI libraries that I don't understand.
-- These should be sufficient to support the simple transaction model
-- understood by most developers.

oci_TRANS_READONLY :: CInt
oci_TRANS_READONLY = 0x00000100  -- 256
oci_TRANS_READWRITE :: CInt
oci_TRANS_READWRITE = 0x00000200  -- 512
oci_TRANS_SERIALIZABLE :: CInt
oci_TRANS_SERIALIZABLE = 0x00000400  -- 1024
