module Lib
    ( libMain
    ) where

import qualified Conduit as C
import Conduit (yieldMany, runConduitPure, mapC, sinkList, ZipSink(..), getZipSink, (.|), lengthC, sumC)

libMain :: IO ()
libMain = putStrLn "someFunc"

ret1 = runConduitPure $ yieldMany [0..255]
  .| getZipSink ((,) <$> ZipSink sumC <*> ZipSink lengthC)

-- ret2 = runConduitPure $ yieldMany [0..255]

