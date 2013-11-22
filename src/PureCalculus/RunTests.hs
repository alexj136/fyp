import TestPureCalculus
import Test.HUnit

main :: IO Counts
main = do
       putStrLn "Running tests for PureCalculus:"
       runTestTT tests
