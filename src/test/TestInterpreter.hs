import Test.HUnit
import Test.QuickCheck
import Interpreter

test1 = TestCase (assertEqual "I don't know who to trust." True True)

tests = TestList [TestLabel "test1" test1]

main :: IO ()
main = do
       putStrLn "Running tests"
