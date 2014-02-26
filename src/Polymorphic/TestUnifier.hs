module TestUnifier where

import Test.HUnit
import Syntax
import Unifier

import qualified Data.Map as M
import Data.Maybe (fromJust)

-- Make a call to deQuantify and retrieve the returned type expression
deQuanType ty = case deQuantify (maxTVar ty + 1) M.empty ty of
    (deQ'dType, int, map) -> deQ'dType

tests = TestList
    [ testInfers
    , testAnotherInfers
    , testBigInfers
    , testSimpleDequantify
    , testBiggerDequantify
    ]

testInfers = TestLabel "Simple tests of type inference" (TestCase (
    assert $ and
        [ infer (Abs "x" (TVar 0) (App (Operation Not) (Var "x"))) == Just (TFunc TBool TBool) -- \x.not x : Bool -> Bool
        , infer (Abs "x" (TVar 0) (App (App (Operation Add) (Constant (IntVal 10))) (Var "x"))) == Just (TFunc TInt TInt) -- \x. 10 + x : Int -> Int
        ]
    ))
testAnotherInfers = TestLabel "Test that λxy.xyz has the correct type inferred" (TestCase (
    assert (
        typeEquiv -- \x.\y.x y : (A -> B) -> A -> B
            (fromJust (infer (AbsInf "x" (AbsInf "y" (App (Var "x") (Var "y"))))))
            (TFunc (TFunc (TVar 1) (TVar 2)) (TFunc (TVar 1) (TVar 2)))
    )))
testBigInfers = TestLabel "Test that λxyz.xz(yz) has the correct type inferred" (TestCase (
    assert (
        typeEquiv -- \x.\y.\y. x z (y z) : (A -> B -> C) -> (A -> B) -> A -> C
            (fromJust (infer (Abs "x" (TVar (-1)) (Abs "y" (TVar (-2)) (Abs "z" (TVar (-3)) (App (App (Var "x") (Var "z")) (App (Var "y") (Var "z"))))))))
            (TFunc (TFunc (TVar 2) (TFunc (TVar 1) (TVar 0))) (TFunc (TFunc (TVar 2) (TVar 1)) (TFunc (TVar 2) (TVar 0))))
    )))
testSimpleDequantify = TestLabel "Simple test of the deQuantify function" (TestCase (
    assert (
        typeEquiv -- 0 -> V0.0 equivalent to 1 -> 0
            (deQuanType (TFunc (TVar 0) (TQuant 0 (TVar 0))))
            (TFunc (TVar 1) (TVar 0))
    )))
testBiggerDequantify = TestLabel "More complex test of the deQuantify function" (TestCase (
    assert (
        typeEquiv -- 0 -> V0.0 -> V0.0 equivalent to 2 -> 1 -> 0
            (deQuanType (TFunc (TVar 0) (TQuant 0 (TFunc (TVar 0) (TQuant 0 (TVar 0))))))
            (TFunc (TVar 2) (TFunc (TVar 1) (TVar 0)))
    )))
