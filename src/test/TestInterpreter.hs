import Test.HUnit
import Test.QuickCheck
import AbstractSyntax
import Interpreter

main :: IO Counts
main = do
       putStrLn "Running tests for Interpreter:"
       runTestTT tests

tests = TestList [
        TestLabel "Test of the identity function" testIdentityFunction,
        TestLabel "Tests that the lambda representations of logical and, true & false reduce correctly" testAnd
    ]

testIdentityFunction =
    TestCase (assert (
        (reduce (Application (Lambda "x" (Name "x")) (Literal 0))) == (Literal 0)
    ))

testAnd = let tRU = Lambda "x" (Lambda "y" (Name "x"))
              fAL = Lambda "x" (Lambda "y" (Name "y"))
              aND = Lambda "x" (Lambda "y" (Application (Application (Name "x") (Name "y")) (fAL))) in
    TestCase (assert (
        and [ reduce (Application (Application aND tRU) tRU) === tRU ,
              reduce (Application (Application aND tRU) fAL) === fAL ,
              reduce (Application (Application aND fAL) tRU) === fAL ,
              reduce (Application (Application aND fAL) fAL) === fAL ]
    ))
