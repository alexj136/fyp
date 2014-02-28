module TestInterpreter where

import Test.HUnit
import Syntax
import Interpreter

-- Integer identity function
iD  = Abs "x" TInt (Var "x")

tests = TestList [ testIdentityFunction
                 , testArithmeticExpression
                 , testListHead
                 , testListTail
                 , testListHeadTail
                 ]

testIdentityFunction = TestLabel "Test of the identity function" (
    TestCase (assert (
        reduceNorm (App iD (Var "Hello!")) === Var "Hello!"
    )))

{--
 - Tests every combination of:
 -     (w op2 x) op1 (y op3 z)
 - where op1, op2 & op3 are +, - and *, and w, x, y and z are integers 0 through 9
 - (there are 270000 combinations).
 --}
testArithmeticExpression = TestLabel "Tests some arithmetic expressions" (
    TestCase (assert $ and
        [
            constToInt (reduceNorm
                (App (App (Operation op1)
                    (App (App (Operation op2)
                        (Constant (IntVal w)))
                        (Constant (IntVal x))))
                    (App (App (Operation op3)
                        (Constant (IntVal y)))
                        (Constant (IntVal z)))))
            ==
            ((realOp op1) (realOp op2 w x) (realOp op3 y z)) 
            | op1 <- ops
            , op2 <- ops
            , op3 <- ops
            , w <- nums
            , x <- nums
            , y <- nums
            , z <- nums
        ]
    ))
    where
        ops :: [OpType]
        ops = [Add, Sub, Mul]

        nums :: [Int]
        nums = [0..9]

        realOp :: (Num a, Integral a) => OpType -> (a -> a -> a)
        realOp Add = (+)
        realOp Sub = (-)
        realOp Mul = (*)
        realOp _   = error "Unsupported Operation"

        constToInt :: TypedExp -> Int
        constToInt (Constant (IntVal x)) = x
        constToInt _ = error "Can't get Int from non-Constant-IntVal TypedExp"

-- Convert a haskell list of expressions into a syntax-encoded list containing
-- the same expressions. Makes unit tests of list functions a lot less verbose.
-- TYPE SAFETY OF THE RESULTING ENCOCDED LISTS IS NOT CHECKED.
listify :: [TypedExp] -> TypedExp
listify []    = Operation Empty
listify (h:t) = App (App (Operation Cons) (h)) (listify t)

testListHead = TestLabel "Test of the list head function" (
    TestCase (assert (
        apply (Operation Head) (listify [Var "Hello", Var "Stuff", Var "Things"]) === Var "Hello"
    )))

testListTail = TestLabel "Test of the list tail function" (
    TestCase (assert (
        apply (Operation Tail) (listify [Var "Hello", Var "Stuff", Var "Things"]) === listify [Var "Stuff", Var "Things"]
    )))

testListHeadTail = TestLabel "Combined test of the list head and tail functions" (
    TestCase (assert (
        apply (Operation Head) (App (Operation Tail) (listify [Var "Hello", Var "Stuff", Var "Things"])) === Var "Stuff"
    )))
