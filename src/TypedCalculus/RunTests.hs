import Test.HUnit
import qualified TestTypeChecker as A
import qualified TestTypedInterpreter as B

main :: IO Counts
main = do
       putStrLn "Running tests for TypeChecker:"
       runTestTT A.tests
       putStrLn "Running tests for TypedInterpreter:"
       runTestTT B.tests
