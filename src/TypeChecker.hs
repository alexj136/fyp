module TypeChecker where

import TypedSyntax

data TypeCheck = Safe TypedExp | TypeError String

type Context = (Name, Type)
