import Test.HUnit
import Test.QuickCheck
import AbstractSyntax

main :: IO Counts
main = do
       putStrLn "Running tests for AbstractSyntax:"
       runTestTT tests

tests = TestList [
        TestLabel "Test of renaming" testRename
        TestLabel "lol" testTrue
    ]

testRename =
    TestCase (assert "Assert that an expression is renamed properly"
       renamedExp === exp && not nameIn "x" exp && not nameIn "y" exp
           where exp = (Lambda "x" (Lambda "y" (Application (Name "x") (Name "y"))))
                 renamedExp = renameAll ["x", "y"] exp
    )
