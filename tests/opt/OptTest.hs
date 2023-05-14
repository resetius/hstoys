module Main where
import Opt
import qualified System.Exit as Exit
import Test.HUnit

test1 :: Test
test1 = TestCase (assertEqual "should return 1" 1 (optFunc 1))

test2 :: Test
test2 = TestCase (assertEqual "should return 2" 2 (optFunc 2))

tests :: Test
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
