module Ex45 (pos1, vel1a, acc1a, vel1n, acc1n, velDifference, plottedVelDifference, plottedAccDifference, vecSum) where

import Graphics.Gnuplot.Plot.TwoDimensional (linearScale)
import Graphics.Gnuplot.Simple (plotFunc)
import Lib (derive)
import Vec

pos1 :: Double -> Double
pos1 t =
  if t < 0
    then 0
    else 5 * t ** 2

vel1a :: Double -> Double
vel1a t =
  if t < 0
    then 0
    else 10 * t

acc1a :: Double -> Double
acc1a t =
  if t < 0
    then 0
    else 10

vel1n :: Double -> Double
vel1n = derive 0.01 pos1

acc1n :: Double -> Double
acc1n = derive 0.01 vel1n

velDifference :: Double -> Double
velDifference x = abs (vel1a x - vel1n x)

accDifference :: Double -> Double
accDifference x = abs (acc1a x - acc1n x)

plottedVelDifference :: IO ()
plottedVelDifference = plotFunc [] (linearScale 1000 (-10, 10 :: Double)) velDifference

plottedAccDifference :: IO ()
plottedAccDifference = plotFunc [] (linearScale 1000 (-10, 10 :: Double)) accDifference

vecSum :: [Vec] -> Vec
vecSum = foldl (^+^) zeroV