import Test.HUnit
import Test.QuickCheck
import AbstractSyntax

main :: IO Counts
main = do
       putStrLn "Running tests for AbstractSyntax:"
       runTestTT tests
--       quickCheck ((\s -> autoRename s === s) :: Expression -> Bool)

tests = TestList [
        TestLabel "Test of renaming" testRename
    ]

-- Asserts that renaming the expression \x.\y.xy produces something
-- alpha-equivalent but not containing the names x or y.
testRename = TestCase (
   let exp = (Abs "x" (Abs "y" (App (Var "x") (Var "y"))))
       renamedExp = renameAll ["x", "y"] exp in
           assert (and [ renamedExp === exp          ,
                         not (nameIn "x" renamedExp) ,
                         not (nameIn "y" renamedExp) ])
    )

-- Changes all the names in an expression
autoRename :: Expression -> Expression
autoRename exp = renameAll (names exp) exp
