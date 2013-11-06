import Test.HUnit
import Test.QuickCheck
import AbstractSyntax
import Interpreter

main :: IO Counts
main = do
       putStrLn "Running tests for Interpreter:"
       runTestTT tests

-- Some useful lambda expressions
tRU = Abs "x" (Abs "y" (Var "x"))
fAL = Abs "x" (Abs "y" (Var "y"))
aND = Abs "x" (Abs "y" (App (App (Var "x") (Var "y")) (fAL)))
oR  = Abs "x" (Abs "y" (App (App (Var "x") (tRU)) (Var "y")))
nOT = Abs "x" (Abs "y" (Abs "z" (App (App (Var "x") (Var "z")) (Var "y"))))

tests = TestList [
        TestLabel "Test of the identity function" testIdentityFunction,
        TestLabel "Tests that the lambda And function works" testAnd,
        TestLabel "Tests that the lambda Or function works" testOr,
        TestLabel "Tests that the lambda Not function works" testNot
    ]

testIdentityFunction =
    TestCase (assert (
        (reduce (App (Abs "x" (Var "x")) (Literal 0))) == (Literal 0)
    ))

testAnd =
    TestCase (assert (
        and [ reduce (App (App aND tRU) tRU) === tRU ,
              reduce (App (App aND tRU) fAL) === fAL ,
              reduce (App (App aND fAL) tRU) === fAL ,
              reduce (App (App aND fAL) fAL) === fAL ]
    ))

testOr =
    TestCase (assert (
        and [ reduce (App (App oR tRU) tRU) === tRU ,
              reduce (App (App oR tRU) fAL) === tRU ,
              reduce (App (App oR fAL) tRU) === tRU ,
              reduce (App (App oR fAL) fAL) === fAL ]
    ))

testNot =
    TestCase (assert (
        and [ reduce (App nOT fAL) === tRU ,
              reduce (App nOT tRU) === fAL ]
    ))
