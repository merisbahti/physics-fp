module Main (main) where

main :: IO ()
main = do
  putStrLn "hello, what's your name?"
  name <- getLine
  putStrLn $ "hey " ++ name
