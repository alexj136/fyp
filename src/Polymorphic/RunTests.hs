import Test.HUnit
import qualified TestSyntax as A
import qualified TestUnifier as B
import qualified TestInterpreter as C

main :: IO Counts
main = do
       putStrLn "Running tests for Syntax:"
       runTestTT A.tests
       putStrLn "Running tests for Unifier:"
       runTestTT B.tests
       putStrLn "Running tests for Interpreter:"
       runTestTT C.tests
