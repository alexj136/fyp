import Test.HUnit
import TypedSyntax
import TypeChecker

main :: IO Counts
main = do
       putStrLn "Running tests for Interpreter:"
       runTestTT tests

-- USEFUL EXPRESSIONS
-- Identity function
intID = Abs "x" (TFunc TInt TInt) (Var "x")

tests = TestList [
        testBlah, testBlahBlah
    ]

testBlah = TestLabel "blah" (TestCase (
    assert True
    ))

testBlahBlah = TestLabel "blah blah" (TestCase (
    assert True
    ))
