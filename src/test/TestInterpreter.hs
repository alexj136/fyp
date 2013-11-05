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
        (reduce (App (Abs "x" (Var "x")) (Literal 0))) == (Literal 0)
    ))

testAnd = let tRU = Abs "x" (Abs "y" (Var "x"))
              fAL = Abs "x" (Abs "y" (Var "y"))
              aND = Abs "x" (Abs "y" (App (App (Var "x") (Var "y")) (fAL))) in
    TestCase (assert (
        and [ reduce (App (App aND tRU) tRU) === tRU ,
              reduce (App (App aND tRU) fAL) === fAL ,
              reduce (App (App aND fAL) tRU) === fAL ,
              reduce (App (App aND fAL) fAL) === fAL ]
    ))
