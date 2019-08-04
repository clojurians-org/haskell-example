{-# LANGUAGE ForeignFunctionInterface #-}
module Network.SSH.Client.LibSSH2.GCrypt 
  ( gcryptFix )
  where

foreign import ccall "gcrypt_fix" gcryptFix :: IO ()

