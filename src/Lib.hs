module Lib
  ( someFunc,
    inc,
    add,
    derive,
  )
where

someFunc :: IO ()
someFunc = putStrLn "somefunk"

derive :: Double -> (Double -> Double) -> (Double -> Double)
derive dx f x = (f (x + (dx / 2)) - f (x - (dx / 2))) / dx

add :: Int -> Int -> Int
add = (+)

inc :: Int -> Int
inc = add 1