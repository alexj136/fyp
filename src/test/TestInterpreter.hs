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

suc = Abs "n" (Abs "f" (Abs "x" (App (App (Var "n") (Var "f")) (Var "x"))))
pow = Abs "b" (Abs "e" (App (Var "e") (Var "b")))

tests = TestList [
        TestLabel "Test of the identity function" testIdentityFunction,
        TestLabel "Tests that the lambda And function works" testAnd,
        TestLabel "Tests that the lambda Or function works" testOr,
        TestLabel "Tests that the lambda Not function works" testNot,
        TestLabel "Tests that the successor function works" testSuccessor,
        TestLabel "Test of natural number exponentiation" testExponentiation
    ]

testIdentityFunction =
    TestCase (assert (
        reduceNorm (App iD (Literal 0)) === Literal 0
    ))

testAnd =
    TestCase (assert (
        and [ reduceNorm (App (App aND tRU) tRU) === tRU ,
              reduceNorm (App (App aND tRU) fAL) === fAL ,
              reduceNorm (App (App aND fAL) tRU) === fAL ,
              reduceNorm (App (App aND fAL) fAL) === fAL ]
    ))

testOr =
    TestCase (assert (
        and [ reduceNorm (App (App oR tRU) tRU) === tRU ,
              reduceNorm (App (App oR tRU) fAL) === tRU ,
              reduceNorm (App (App oR fAL) tRU) === tRU ,
              reduceNorm (App (App oR fAL) fAL) === fAL ]
    ))

testNot =
    TestCase (assert (
        and [ reduceNorm (App nOT fAL) === tRU ,
              reduceNorm (App nOT tRU) === fAL ]
    ))

testSuccessor =
    TestCase (assert (
        and $ map (\x -> (reduceNorm (App suc (lamInt x)) === lamInt (x+1))) [0, 4 .. 80]
    ))

testExponentiation =
    TestCase (assert (
        and [ reduceNorm (App (App (pow) (lamInt 2)) (lamInt 2)) === lamInt 4  ,
              reduceNorm (App (App (pow) (lamInt 3)) (lamInt 3)) === lamInt 27 ,
              reduceNorm (App (App (pow) (lamInt 3)) (lamInt 4)) === lamInt 81 ]
    ))
