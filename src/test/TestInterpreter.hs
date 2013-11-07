import Test.HUnit
import Test.QuickCheck
import AbstractSyntax
import Interpreter

main :: IO Counts
main = do
       putStrLn "Running tests for Interpreter:"
       runTestTT tests

-- Some useful lambda expressions
iD  = Abs "x" (Var "x")
-- Booleans
tRU = Abs "x" (Abs "y" (Var "x"))
fAL = Abs "x" (Abs "y" (Var "y"))
aND = Abs "x" (Abs "y" (App (App (Var "x") (Var "y")) (fAL)))
oR  = Abs "x" (Abs "y" (App (App (Var "x") (tRU)) (Var "y")))
nOT = Abs "x" (Abs "y" (Abs "z" (App (App (Var "x") (Var "z")) (Var "y"))))
-- Functions to generate integers
lamInt :: Int -> Expression
lamInt n = Abs "f" (Abs "x" (nApps n))

nApps :: Int -> Expression
nApps n | n  < 0 = error "Cannot generate negative number"
        | n == 0 = Var "x"
        | n  > 0 = App (Var "f") (nApps (n - 1))

-- Exponentiation
pow = Abs "b" (Abs "e" (App (Var "e") (Var "b")))

tests = TestList [
        TestLabel "Test of the identity function" testIdentityFunction,
        TestLabel "Tests that the lambda And function works" testAnd,
        TestLabel "Tests that the lambda Or function works" testOr,
        TestLabel "Tests that the lambda Not function works" testNot,
        TestLabel "Test of natural number exponentiation" testExponentiation
    ]

testIdentityFunction =
    TestCase (assert (
        reduce (App iD (Literal 0)) === Literal 0
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

testExponentiation =
    TestCase (assert (
        and [ reduce (App (App (pow) (lamInt 2)) (lamInt 2)) === lamInt 4 ,
              reduce (App (App (pow) (lamInt 3)) (lamInt 3)) === lamInt 27 ]
    ))
