module TestUnifier where

import Test.HUnit
import PolymorphicSyntax
import Unifier

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)

tests = TestList
    [ testInfers
    , testBlahBlah
    ]

testInfers = TestLabel "Simple tests of type inference" (TestCase (
    assert $ and
        [ infer (Abs "x" (TVar (-1)) (App (UnaryOp Not) (Var "x"))) == Just (TFunc TBool TBool) -- \x.not x : Bool -> Bool
        , infer (Abs "x" (TVar (-1)) (App (App (BinaryOp Add) (Constant (IntVal 10))) (Var "x"))) == Just (TFunc TInt TInt) -- \x. 10 + x : Int -> Int
        ]
    ))

testBlahBlah = TestLabel "blah blah" (TestCase (
    assert $ and
        [ True
        , True
        , True
        , True
        ]
    ))
