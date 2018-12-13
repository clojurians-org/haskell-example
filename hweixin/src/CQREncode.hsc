{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module CQREncode where

import Foreign.Ptr (Ptr)
import Foreign.C.Types (CChar(..), CInt(..), CLong(..), CBool(..), CFile(..))
import Foreign.C.String (CString)
import Foreign.Storable (Storable, sizeOf, alignment, peek, poke, peekByteOff, pokeByteOff)

#include <qrencode.h>

-- QRecLevel
newtype QREncodeLevel = QREncodeLevel { unQREncodeLevel :: CInt } deriving (Eq,Show)
#{enum QREncodeLevel, QREncodeLevel
 , qr_eclevel_l = QR_ECLEVEL_L
 , qr_eclevel_m = QR_ECLEVEL_M
 , qr_eclevel_q = QR_ECLEVEL_Q
 , qr_eclevel_h = QR_ECLEVEL_H
 }

-- QRencodeMode
newtype QREncodeMode = QREncodeMode { unQREncodeMode :: CInt } deriving (Eq,Show)
#{enum QREncodeMode, QREncodeMode
 , qr_mode_num = QR_MODE_NUM
 , qr_mode_an = QR_MODE_AN
 , qr_mode_eight = QR_MODE_8
 , qr_mode_kanji = QR_MODE_KANJI
 }

-- QRVersion
newtype QRVersion = QRVersion { unQRVersion :: CInt } deriving (Eq,Show)
#{enum QRVersion, QRVersion
 , qr_version_auto = 0
 , qr_v01 = 1
 , qr_v02 = 2
 , qr_v03 = 3
 , qr_v04 = 4
 }

newtype QRCaseMode = QRCaseMode { unQRCaseMode :: CInt } deriving (Eq,Show)
#{enum QRCaseMode, QRCaseMode
 , qr_case_on = 1
 , qr_case_off = 0
 }

-- QRCode
data CQRcode = CQRcode {
   qr_code_version :: CInt
 , qr_code_width :: CInt
 , qr_code_data :: CString
} deriving (Show)

instance Storable CQRcode where
  sizeOf _ = #{size QRcode}
  alignment _ = #{alignment QRcode}
  peek ptr = do
    qr_code_version <- #{peek QRcode, version} ptr
    qr_code_width <- #{peek QRcode, width} ptr
    qr_code_data <- #{peek QRcode, data} ptr
    return $ CQRcode qr_code_version qr_code_width qr_code_data
    
  poke ptr (CQRcode qr_code_version qr_code_width qr_code_data) = do
     #{poke QRcode, version} ptr qr_code_version
     #{poke QRcode, width} ptr qr_code_width
     #{poke QRcode, data} ptr qr_code_data
    
-- extern char *QRcode_APIVersionString(void);
foreign import ccall unsafe "QRcode_APIVersionString"
  qr_apiVersionString :: IO CString

-- extern QRcode *QRcode_encodeString(const char *string, int version, QRecLevel level, QRencodeMode hint, int casesensitive);
foreign import ccall unsafe "QRcode_encodeString"
  qr_encodeString :: CString
                 -> QRVersion
                 -> QREncodeLevel
                 -> QREncodeMode
                 -> QRCaseMode
                 -> IO (Ptr CQRcode)

-- extern void QRcode_free(QRcode *qrcode);
foreign import ccall unsafe "QRcode_free"
  qr_free :: Ptr CQRcode -> IO ()
