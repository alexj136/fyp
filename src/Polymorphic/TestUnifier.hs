module TestUnifier where

import Test.HUnit
import PolymorphicSyntax
import Unifier

import Data.Maybe (fromJust)

tests = TestList
    [ testInfers
    , testBigInfers
    ]

testInfers = TestLabel "Simple tests of type inference" (TestCase (
    assert $ and
        [ infer (Abs "x" (TVar (-1)) (App (UnaryOp Not) (Var "x"))) == Just (TFunc TBool TBool) -- \x.not x : Bool -> Bool
        , infer (Abs "x" (TVar (-1)) (App (App (BinaryOp Add) (Constant (IntVal 10))) (Var "x"))) == Just (TFunc TInt TInt) -- \x. 10 + x : Int -> Int
        ]
    ))

testBigInfers = TestLabel "Test that λxyz.xz(yz) has the correct type inferred" (TestCase (
    assert (
        typeEquiv
            (fromJust (infer (Abs "x" (TVar (-1)) (Abs "y" (TVar (-2)) (Abs "z" (TVar (-3)) (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z"))))))))
            (TFunc (TFunc (TVar 2) (TFunc (TVar 1) (TVar 0))) (TFunc (TFunc (TVar 2) (TVar 1)) (TFunc (TVar 2) (TVar 0))))
    )))
