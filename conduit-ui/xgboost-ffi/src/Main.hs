module Main where

import XGBoostFFI
import Control.Monad (forM_)

main :: IO ()
main = putStrLn "Hello, Haskell!"

repl :: IO ()
repl = do
  (_, dmat) <- dmCreateFromMat [1.0, 2.0, 3.0, 4.0] 2 2 2.0
  dmSetFloatInfo dmat "label" [5.0, 6.0]
  (_, info) <- dmGetFloatInfo dmat "label"
  putStrLn (show info)
  dmFree dmat

  (_, dTrain) <- dmCreateFromFile "data/agaricus.txt.train" 0
  (_, dTest) <- dmCreateFromFile "data/agaricus.txt.test" 0

  (_, booster) <- boosterCreate [dTrain]
--  boosterSetParam boosterH "booster" "gbtree"
--  boosterSetParam booster "seed" "0"
  boosterSetParam booster "max_depth" "2"
  boosterSetParam booster "eta" "1"
  boosterSetParam booster "slient" "1"
  boosterSetParam booster "objective" "binary:logistic"
--  boosterSetParam boosterH "min_child_weight" "1"
--  boosterSetParam boosterH "subsample" "0.5"
--  boosterSetParam boosterH "colsample_bytree" "1"
--  boosterSetParam boosterH "num_parallel_tree" "1"

  forM_ [1..2] $ \n -> boosterUpdateOnceIter booster n dTrain
  (_, result) <- boosterPredict booster dTest 0 0
  putStrLn (show (take 5 result))
  
  dmFree dTrain
  dmFree dTest
  putStrLn "finished"
