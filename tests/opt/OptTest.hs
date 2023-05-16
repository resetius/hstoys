module Main where
import Opt
import qualified System.Exit as Exit
import Test.HUnit

test1 :: Test
test1 = TestCase (assertEqual "should return 1" 1 (optFunc 1))

test2 :: Test
test2 = TestCase (assertEqual "should return 2" 2 (optFunc 2))

test3 :: Test
test3 = TestCase (assertEqual "test3" [1,0] (scanrr (\x y z -> x + y) 0 [1]))

test4 :: Test
test4 = TestCase (assertEqual "test4" [3,2,0] (scanrr (\x y z -> x + y) 0 [1,2]))

test5 :: Test
test5 = TestCase (assertEqual "test5" [10,9,7,4,0] (scanrr (\x y z -> x + y) 0 [1..4]))

test6 :: Test
test6 = TestCase (assertEqual "test6" [42] (scanrr (\x y z -> x + y) 42 []))

test7 :: Test
test7 = TestCase (assertEqual "splits" [([2,1],[]),([1],[2]),([],[1,2])] (splits [1, 2]))

test8 :: Test
test8 = TestCase (assertEqual "splits" [([3,2,1],[]),([2,1],[3]),([1],[2,3]),([],[1,2,3])] (splits [1, 2, 3]))

test9 :: Test
test9 = TestCase (assertEqual "splits" [([4,3,2,1],[]),([3,2,1],[4]),([2,1],[3,4]),([1],[2,3,4]),([],[1,2,3,4])] (splits [1, 2, 3, 4]))

tests :: Test
tests = TestList [
    TestLabel "test1" test1,
    TestLabel "test2" test2,
    TestLabel "test3" test3,
    TestLabel "test4" test4,
    TestLabel "test5" test5,
    TestLabel "test6" test6,
    TestLabel "test7" test7,
    TestLabel "test8" test8,
    TestLabel "test9" test9
    ]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
