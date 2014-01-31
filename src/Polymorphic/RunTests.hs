import Test.HUnit
import qualified TestSyntax as A
import qualified TestUnifier as B

main :: IO Counts
main = do
       putStrLn "Running tests for Syntax:"
       runTestTT A.tests
       putStrLn "Running tests for Unifier:"
       runTestTT B.tests
