import Test.HUnit
import TypedSyntax
import TypeChecker

import qualified Data.Map as M

main :: IO Counts
main = do
       putStrLn "Running tests for Interpreter:"
       runTestTT tests

-- USEFUL EXPRESSIONS
-- Identity function
intID = Abs "x" TInt (Var "x")

tests = TestList [
        testIntID, testBlahBlah
    ]

testIntID = TestLabel "Some simple cases with an integer ID function" (TestCase (
    assert $ and
        [ typeOf M.empty (App intID (Constant (IntVal 0))) == TInt
        , typeOf (contextFrom "x" TInt) intID == TFunc TInt TInt
        ]
    ))

testBlahBlah = TestLabel "blah blah" (TestCase (
    assert $ and
        [ typeOf M.empty (App intID (Constant (IntVal 0))) == TInt
        , typeOf (contextFrom "x" TInt) intID == TFunc TInt TInt
        ]
    ))
