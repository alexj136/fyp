import Test.HUnit
import Interpreter
import AbstractSyntax

test1 = TestCase (assertEqual (reduce (Application (Lambda "x" (Lambda "y" (Name "y"))) (Name "y"))) )

tests = TestList [TestLabel "test1" test1]
