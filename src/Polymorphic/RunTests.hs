import Test.HUnit
import qualified TestUnifier as A

main :: IO Counts
main = do
       putStrLn "Running tests for Unifier:"
       runTestTT A.tests
