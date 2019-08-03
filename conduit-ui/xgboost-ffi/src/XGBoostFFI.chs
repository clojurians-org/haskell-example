{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module XGBoostFFI where

import Control.Monad (liftM)
import Foreign
import Foreign.C

import Control.Monad (forM_)
#include <xgboost/c_api.h>

peekPtr :: Ptr a -> IO (Ptr b)
peekPtr = peek . castPtr

peekIntConv :: (Storable a, Integral a, Integral b) => Ptr a -> IO b
peekIntConv = liftM fromIntegral . peek
peekFloatConv :: (Storable a, RealFloat a, RealFloat b) => Ptr a -> IO b
peekFloatConv  = liftM realToFrac . peek

type DMatrix = Ptr Word8
type Booster = Ptr Word8
{#pointer *DMatrixHandle as DMatrixPtr foreign -> DMatrix #}
{#pointer *BoosterHandle as BoosterPtr foreign -> Booster #}

{#fun XGBGetLastError as cGetLastError {}  -> `String' #}
getLastError :: IO String
getLastError = cGetLastError

{#fun XGDMatrixCreateFromMat as cDMatrixCreateFromMat
   { castPtr `Ptr Float'
   , `Int'
   , `Int'
   , `Float'
   , alloca- `DMatrix' peekPtr* }
   -> `Int' #}
dmCreateFromMat :: [Float] -> Int -> Int -> Float -> IO (Int, DMatrix)
dmCreateFromMat xs nrow ncol missing = do
 withArray xs $ \xsPtr -> cDMatrixCreateFromMat xsPtr nrow ncol missing


{#fun XGDMatrixCreateFromFile as cDMatrixCreateFromFile
   { `String'
   , `Int'
   , alloca- `DMatrix' peekPtr* }
   -> `Int' #}
dmCreateFromFile :: FilePath -> Int -> IO (Int, DMatrix)
dmCreateFromFile = cDMatrixCreateFromFile


{#fun XGDMatrixFree as cDMatrixFree { castPtr `DMatrix' }  -> `Int' #}
dmFree :: DMatrix -> IO Int
dmFree = cDMatrixFree

{#fun XGDMatrixSetFloatInfo as cDMatrixSetFloatInfo
   { castPtr `DMatrix'
   , `String'
   , castPtr `Ptr Float'
   , `Int'}
   -> `Int' #}
dmSetFloatInfo :: DMatrix -> String -> [Float] -> IO Int
dmSetFloatInfo dm field xs =
  withArray xs $ \xsPtr -> cDMatrixSetFloatInfo dm field xsPtr (length xs)

{#fun XGDMatrixGetFloatInfo as cDMatrixGetFloatInfo
   { castPtr `DMatrix'
   , `String'
   , alloca- `Int' peekIntConv*
   , alloca- `Ptr Float' peekPtr* }
   -> `Int' #}
dmGetFloatInfo :: DMatrix -> String -> IO (Int, [Float])
dmGetFloatInfo dm field = do
  (ret, outLen, outDPtr) <- cDMatrixGetFloatInfo dm field
  (ret,) <$> peekArray outLen outDPtr

{#fun XGBoosterCreate as cBoosterCreate
   { castPtr `Ptr DMatrix'
   , `Int'
   , alloca- `Booster' peekPtr* }
   -> `Int' #}
boosterCreate :: [DMatrix] -> IO (Int, Booster)
boosterCreate xs =
  withArray xs $ \xsPtr -> cBoosterCreate xsPtr (length xs)

{#fun XGBoosterSetParam as cBoosterSetParam
   { castPtr `Booster'
   , `String'
   , `String'}
   -> `Int' #}
boosterSetParam :: Booster -> String -> String -> IO Int
boosterSetParam = cBoosterSetParam

{#fun XGBoosterUpdateOneIter as cBoosterUpdateOneIter
   { castPtr `Booster'
   , `Int'
   , castPtr `DMatrix'}
   -> `Int' #}
boosterUpdateOnceIter :: Booster -> Int -> DMatrix -> IO Int
boosterUpdateOnceIter = cBoosterUpdateOneIter

{#fun XGBoosterPredict as cBoosterPredict
   { castPtr `Booster'
   , castPtr `DMatrix'
   , `Int'
   , `Int'
   , alloca- `Int' peekIntConv*
   , alloca- `Ptr Float' peekPtr* }
   -> `Int' #}
boosterPredict :: Booster -> DMatrix -> Int -> Int -> IO (Int, [Float])
boosterPredict booster dmat maskOpt ntreeLimit = do
  (ret, outLen, outPtr) <- cBoosterPredict booster dmat maskOpt ntreeLimit
  (ret,) <$> peekArray outLen outPtr
