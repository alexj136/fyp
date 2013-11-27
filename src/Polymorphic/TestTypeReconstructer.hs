module TestTypeReconstructer where

import Test.HUnit
import TypeReconstructer

import qualified Data.Map as M

-- USEFUL EXPRESSIONS
-- Identity function
--intID = Abs "x" TInt (Var "x")

tests = TestList [
        testBlah, testBlahBlah
    ]

testBlah = TestLabel "blah" (TestCase (
    assert $ and
        [ True
        , True
        ]
    ))

testBlahBlah = TestLabel "blah blah" (TestCase (
    assert True
    ))
