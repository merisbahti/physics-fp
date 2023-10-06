module Lib
  ( someFunc,
    inc,
    add,
  )
where

someFunc :: IO ()
someFunc = putStrLn "somefunk"

add :: Int -> Int -> Int
add = (+)

inc :: Int -> Int
inc = add 1