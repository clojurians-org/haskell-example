{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Main where

import Data.Text

import Data.Default.Class (def)
import Data.Function ((&))
import Control.Lens ((.~), (.=), (%~), view, over)

import System.IO
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Data.Vinyl (RecElem(..))
import Pipes (Producer, runEffect, (>->))
import Pipes.Safe (SafeT, runSafeT)
import Frames (Frame, Record, RecordColumns, (:->)
             , tableTypes, readTable, inCoreAoS
             , frameRow, pipePreview
             , rcast, rsubset, mapMono)
import Frames.Joins ()
import qualified Pipes.Prelude as P
import Graphics.Rendering.Chart.Easy (
    Renderable(..), ToPlot(toPlot), EC(..), Layout(..)
  , toRenderable, toPlot, plot, plotLeft, plotRight, line
  , layout_title
  , layoutlr_title, layoutlr_x_axis, layoutlr_left_axis, layoutlr_right_axis
  , layoutlr_plots, layoutlr_grid_last
  , laxis_override, axisGridHide
  , plot_lines_title, plot_lines_values
  )
import Graphics.Rendering.Chart.Backend.Diagrams (renderableToFile, toFile)

import Data.Vinyl (Rec(..), ElField(..), getField, rlens)
-- import Data.Vinyl

main :: IO ()
main = do
  
  let xs1 = [[(1, 1.0) , (2, 2.0), (3, 3.0), (4, 4.0)]] :: [[(Int, Double)]]
  let xs2 = [[(1, 1.0) , (2, 2.0), (3, 4.0), (4, 8.0)]] :: [[(Int, Double)]]
  toFile def "first.svg" $ do
    layoutlr_title .= "Price History"
    layoutlr_left_axis . laxis_override .= axisGridHide
    layoutlr_right_axis . laxis_override .= axisGridHide
    plotLeft (line "price 1" xs1)
    plotRight (line "price 2" xs2)
    
  toFile def "second.svg" $ do
    layout_title .= "Simulation of betting on a biased coin"
    plot (line "f=0.5" xs1)
  putStrLn "finished"


tableTypes "Row" "data/prestige.csv"

type RStream r = Producer r (SafeT IO) ()
type NewCol name = forall s. s -> ElField '(name, s)

repl :: IO ()
repl = do
  let rows :: RStream Row = readTable "data/prestige.csv"
  rows' <- runSafeT . P.toListM $ rows
             -- | 1. add constant
             >-> P.map ((:&) <$>  const ((Field :: NewCol "f1") @Int 5) <*> id)
             -- | 2. add derived field
             >-> P.map ((:&) <$> ((Field :: NewCol "f2") . (+2) . getField . view (rlens @Income) ) <*> id)
             -- | 3. modify field
             >-> P.map (over (rlens @Income) (+1))
             -- | 4. keep field
             >-> P.map (rcast @["f1" :-> Int, "f2" :-> Int, Education, Income])
             >-> P.take 3
  mapM_ print rows'          
  putStrLn "finished"
