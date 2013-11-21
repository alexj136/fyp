import Test.HUnit
import TypedSyntax
import TypedInterpreter

main :: IO Counts
main = do
       putStrLn "Running tests for TypedInterpreter:"
       runTestTT tests

-- Integer identity function
iD  = Abs "x" TInt (Var "x")

tests = TestList [
        testIdentityFunction
    ]

testIdentityFunction = TestLabel "Test of the identity function" (
    TestCase (assert (
        reduceNorm (App iD (Var "Hello!")) === Var "Hello!"
    )))
