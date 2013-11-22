module TestTypedInterpreter where

import Test.HUnit
import TypedSyntax
import TypedInterpreter

-- Integer identity function
iD  = Abs "x" TInt (Var "x")

tests = TestList [ testIdentityFunction
                 , testArithmeticExpression
                 ]

testIdentityFunction = TestLabel "Test of the identity function" (
    TestCase (assert (
        reduceNorm (App iD (Var "Hello!")) === Var "Hello!"
    )))

{--
 - Tests every combination of:
 -     (w op2 x) op1 (y op3 z)
 - where op1, op2 & op3 are +, - % *, and w, x, y and z are integers 0 through 9
 - (there are 270000 combinations).
 --}
testArithmeticExpression = TestLabel "Tests some arithmetic expressions" (
    TestCase (assert $ and
        [
            constToInt (reduceNorm
                (App (App (BinaryOp op1)
                    (App (App (BinaryOp op2)
                        (Constant (IntVal w)))
                        (Constant (IntVal x))))
                    (App (App (BinaryOp op3)
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
    where ops :: [BinaryOpType]
          ops = [Add, Sub, Mul]
          nums :: [Int]
          nums = [0..9]
          realOp :: (Num a, Integral a) => BinaryOpType -> (a -> a -> a)
          realOp Add = (+)
          realOp Sub = (-)
          realOp Mul = (*)
          realOp Div = div
          realOp Mod = mod
          realOp _   = error "Unsupported BinaryOpType"
          constToInt :: TypedExp -> Int
          constToInt (Constant (IntVal x)) = x
          constToInt _ = error "Can't get Int from non-Constant-IntVal TypedExp"
