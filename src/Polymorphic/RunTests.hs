import Test.HUnit
import qualified TestTypeReconstructer as A

main :: IO Counts
main = do
       putStrLn "Running tests for TypeReconstructer:"
       runTestTT A.tests
