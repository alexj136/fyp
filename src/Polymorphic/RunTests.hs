import Test.HUnit
import qualified TestUnifier as A
import qualified TestPolymorphicSyntax as B

main :: IO Counts
main = do
       putStrLn "Running tests for Unifier:"
       runTestTT A.tests
       putStrLn "Running tests for PolymorphicSyntax:"
       runTestTT B.tests
