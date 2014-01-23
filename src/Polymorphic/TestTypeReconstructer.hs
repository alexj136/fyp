module TestTypeReconstructer where

import Test.HUnit
import PolymorphicSyntax
import TypeReconstructer

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromJust)

tests = TestList
    [ testSimpleUnifies
    , testBlahBlah
    ]

testSimpleUnifies = TestLabel "Simple tests of unify" (TestCase (
    assert $ and
        [ fromJust (unify (newConstrSet (TVar 0) TInt)) (TVar 0) == TInt
        , fromJust (unify (S.fromList
            [ ( TVar 0 , TVar 1 )
            , ( TVar 3 , TVar 4 )
            , ( TVar 2 , TVar 3 )
            , ( TVar 4 , TVar 5 )
            , ( TVar 5 , TInt   )
            , ( TVar 1 , TVar 2 )
            ] )) (TVar 0) == TInt
        , fromJust (unify (S.fromList
            [ ( TVar 0 , TVar 1                  )
            , ( TVar 1 , TVar 2                  )
            , ( TVar 2 , TVar 3                  )
            , ( TVar 3 , TVar 4                  )
            , ( TVar 4 , TVar 5                  )
            , ( TVar 5 , TInt                    )
            , ( TVar 6 , TVar 7                  )
            , ( TVar 7 , TVar 8                  )
            , ( TVar 8 , TBool                   )
            , ( TVar 9 , TFunc (TVar 0) (TVar 6) )
            ] )) (TVar 9) == TFunc TInt TBool
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
