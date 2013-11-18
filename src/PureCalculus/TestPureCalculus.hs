import Test.HUnit
import PureSyntax
import PureInterpreter

main :: IO Counts
main = do
       putStrLn "Running tests for Interpreter:"
       runTestTT tests

-- USEFUL LAMBDA EXPRESSIONS
-- Identity function
iD  = Abs "x" (Var "x")

-- Boolean constants & functions
tRU = Abs "x" (Abs "y" (Var "x"))
fAL = Abs "x" (Abs "y" (Var "y"))
aND = Abs "x" (Abs "y" (App (App (Var "x") (Var "y")) (fAL)))
oR  = Abs "x" (Abs "y" (App (App (Var "x") (tRU)) (Var "y")))
nOT = Abs "x" (Abs "y" (Abs "z" (App (App (Var "x") (Var "z")) (Var "y"))))

-- Function that generates church numeral natural numbers
lamInt :: Int -> Expression
lamInt n = Abs "f" (Abs "x" (nApps n))
    where nApps n | n  < 0 = error "Cannot generate negative number"
                  | n == 0 = Var "x"
                  | n  > 0 = App (Var "f") (nApps (n - 1))

-- Arithmetic operations
suc = Abs "n" (Abs "f" (Abs "x" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x")))))
plu = Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (App (Var "m") (Var "f")) (App (App (Var "n") (Var "f")) (Var "x"))))))
pow = Abs "b" (Abs "e" (App (Var "e") (Var "b")))

yco = App (Abs "g" (Abs "x" (App (Var "g") (App (Var "x") (Var "x"))))) (Abs "x" (App (Var "g") (App (Var "x") (Var "x"))))

tests = TestList [
        testRename,    testIdentityFunction, testAnd, testOr, testNot,
        testSuccessor, testExponentiation,   testPlus
    ]

-- Asserts that renaming the expression \x.\y.xy produces something
-- alpha-equivalent but not containing the names x or y.
testRename = TestLabel "Test of renaming" (TestCase (
    let exp = (Abs "x" (Abs "y" (App (Var "x") (Var "y"))))
        renamedExp = renameAll ["x", "y"] exp in
            assert (and [ renamedExp === exp          ,
                          not (nameIn "x" renamedExp) ,
                          not (nameIn "y" renamedExp) ])
    ))

testIdentityFunction = TestLabel "Test of the identity function" (
    TestCase (assert (
        reduceNorm (App iD (Var "Hello!")) === Var "Hello!"
    )))

testAnd = TestLabel "Tests that the lambda And function works" (
    TestCase (assert (
        and [ reduceNorm (App (App aND tRU) tRU) === tRU ,
              reduceNorm (App (App aND tRU) fAL) === fAL ,
              reduceNorm (App (App aND fAL) tRU) === fAL ,
              reduceNorm (App (App aND fAL) fAL) === fAL ]
    )))

testOr = TestLabel "Tests that the lambda Or function works" (
    TestCase (assert (
        and [ reduceNorm (App (App oR tRU) tRU) === tRU ,
              reduceNorm (App (App oR tRU) fAL) === tRU ,
              reduceNorm (App (App oR fAL) tRU) === tRU ,
              reduceNorm (App (App oR fAL) fAL) === fAL ]
    )))

testNot = TestLabel "Tests that the lambda Not function works" (
    TestCase (assert (
        and [ reduceNorm (App nOT fAL) === tRU ,
              reduceNorm (App nOT tRU) === fAL ]
    )))

testSuccessor = TestLabel "Tests that the successor function works" (
    TestCase (assert (
        and $ map (\x -> (reduceNorm (App suc (lamInt x)) === lamInt (x+1))) [0, 3 .. 60]
    )))

testExponentiation = TestLabel "Test of natural number exponentiation" (
    TestCase (assert (
        and [ reduceNorm (App (App (pow) (lamInt 2)) (lamInt 2)) === lamInt 4  ,
              reduceNorm (App (App (pow) (lamInt 3)) (lamInt 3)) === lamInt 27 ,
              reduceNorm (App (App (pow) (lamInt 3)) (lamInt 4)) === lamInt 81 ]
    )))

testPlus = TestLabel "Test of natural number addition" (
    TestCase (assert (
        and $ map (\x -> reduceNorm (App (App plu (lamInt $ fst x)) (lamInt $ snd x)) === lamInt (fst x + snd x)) [(i, j) | i <- [1..20], j <- [1..20]]
    )))
