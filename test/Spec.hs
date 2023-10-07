import Test.HUnit

main :: IO Counts
main = runTestTT tests

tests :: Test
tests =
  TestList
    [ TestCase (assertEqual "something" "ab" ("a" ++ "bc"))
    ]
