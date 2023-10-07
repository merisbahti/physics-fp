module Main (main) where

import Graphics.Gnuplot.Simple

xs :: [Double]
xs = [0, 0.1 .. 10]

main :: IO ()
main = do
  putStrLn "hello, what's your name?"
  name <- getLine
  putStrLn $ "hey " ++ name
