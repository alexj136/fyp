import Test.HUnit
import TypedSyntax
import TypeChecker

import qualified Data.Map as M

main :: IO Counts
main = do
       putStrLn "Running tests for TypeChecker:"
       runTestTT tests

-- USEFUL EXPRESSIONS
-- Identity function
intID = Abs "x" TInt (Var "x")

tests = TestList [
        testIntID, testOps, testSomeExpressions
    ]

testIntID = TestLabel "Some simple cases with an integer ID function" (TestCase
    (assert $ and
        [ typeOf M.empty (App intID (Constant (IntVal 0))) == TInt
        , typeOf (contextFrom "x" TInt) intID == TFunc TInt TInt
        ]
    ))

testOps = TestLabel "Tests that the binary and unary ops have the correct types"
    (TestCase (assert $ and
        [ typeOf M.empty (BinaryOp Add) == TFunc TInt  (TFunc TInt  TInt)
        , typeOf M.empty (BinaryOp Sub) == TFunc TInt  (TFunc TInt  TInt)
        , typeOf M.empty (BinaryOp Mul) == TFunc TInt  (TFunc TInt  TInt)
        , typeOf M.empty (BinaryOp Div) == TFunc TInt  (TFunc TInt  TInt)
        , typeOf M.empty (BinaryOp Mod) == TFunc TInt  (TFunc TInt  TInt)
        , typeOf M.empty (BinaryOp And) == TFunc TBool (TFunc TBool TBool)
        , typeOf M.empty (BinaryOp Or ) == TFunc TBool (TFunc TBool TBool)
        , typeOf M.empty (BinaryOp Xor) == TFunc TBool (TFunc TBool TBool)
        , typeOf M.empty (UnaryOp  IsZ) == TFunc TInt  TBool
        , typeOf M.empty (UnaryOp  Not) == TFunc TBool TBool
        ]
    ))

testSomeExpressions = TestLabel "Tests some expressions" (TestCase (
    assert $ and
        [ typeOf M.empty (App (App (Abs "x" TBool (Abs "y" TBool (App (App (BinaryOp And) (Var "x")) (Var "y")))) (Constant (BoolVal True))) (Constant (BoolVal False))) == TBool
        , typeOf M.empty (Abs "x" TBool (Constant (IntVal 0))) == TFunc TBool TInt
        , typeOf M.empty (Abs "x" TBool (Abs "y" TInt (Constant (IntVal 0)))) == TFunc TBool (TFunc TInt TInt)
        , typeOf (contextFrom "z" TBool) (Abs "x" TBool (Abs "y" TInt (Var "z"))) == TFunc TBool (TFunc TInt TBool)
        ]
    ))
