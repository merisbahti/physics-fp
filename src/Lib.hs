module Lib
  ( R,
    derive,
    Time,
  )
where

type R = Double

type Time = R

derive :: Double -> (Double -> Double) -> (Double -> Double)
derive dx f x = (f (x + (dx / 2)) - f (x - (dx / 2))) / dx
