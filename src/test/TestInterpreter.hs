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
        TestLabel "lol" testTrue
    ]

testIdentityFunction =
    TestCase (assertEqual "Tests that the identity function reduces properly"
        (reduce (Application (Lambda "x" (Name "x")) (Literal 0)))
        (Literal 0)
    )

testAnd = let tRU = Lambda "x" (Lambda "y" (Name "x"))
              fAL = Lambda "x" (Lambda "y" (Name "y"))
              aND = Lambda "x" (Lambda "y" (Application (Name "x") (Application (Name "y") (fAL))))
    TestCase (assert (
        and [
            reduce (Application () ())
        ]
    ))
