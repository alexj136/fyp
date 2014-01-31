module TestSyntax where

import Test.HUnit
import Syntax

tests = TestList
    [ testTypeEquiv
    ]

testTypeEquiv = TestLabel "Tests of the typeEquiv function" (TestCase (
    assert $ and
        [      typeEquiv (TFunc (TFunc (TVar 1) (TVar 1)) (TVar 2)) (TFunc (TFunc (TVar 2) (TVar 2)) (TVar 1))
        , not (typeEquiv (TFunc (TFunc (TVar 1) (TVar 1)) (TVar 2)) (TFunc (TFunc (TVar 1) (TVar 2)) (TVar 1)))
        ,      typeEquiv (TVar 1) (TVar 2)
        , not (typeEquiv (TVar 1) TInt)
        ]
    ))
