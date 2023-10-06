module Lib
  ( someFunc,
    inc,
  )
where

someFunc :: IO ()
someFunc = putStrLn "somefunk"

inc :: Int -> Int
inc = (+) 1