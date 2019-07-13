module Database.Dpi.Prelude
  ( module Foreign
  , module Foreign.C.String
  , module Foreign.C.Types
  , module Database.Dpi.Prelude
  , join
  , (<>)
  , (&)
  , HasCallStack
  , ByteString
  ) where

import           Control.Monad    (join)
import           Data.ByteString  (ByteString)
import qualified Data.ByteString  as B
import           Data.Function    ((&))
import           Data.Monoid      ((<>))
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types
import           GHC.Stack

{-# INLINE toMaybePtr #-}
toMaybePtr :: Ptr a -> Maybe (Ptr a)
toMaybePtr p = if p == nullPtr then Nothing else Just p

{-# INLINE ts #-}
ts :: CString -> IO ByteString
ts p = if p == nullPtr then return B.empty else B.packCString p

{-# INLINE tsLen #-}
tsLen :: CStringLen -> IO ByteString
tsLen p@(p',_) = if p' == nullPtr then return B.empty else B.packCStringLen p

{-# INLINE fe #-}
fe :: (Enum e, Integral n) => e -> n
fe = fromIntegral . fromEnum

{-# INLINE te #-}
te :: (Integral n, Enum e) => n -> e
te = toEnum . fromIntegral

{-# INLINE ft #-}
ft :: (Integral a, Num b) => a -> b
ft = fromInteger . toInteger

{-# INLINE noImplement #-}
noImplement :: HasCallStack => a
noImplement = error "Not supported yet"
