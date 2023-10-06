import Lib

main :: IO ()
main = print . inc $ inc $ add 2 2
