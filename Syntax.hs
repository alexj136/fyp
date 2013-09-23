-- This module defines the abstract syntax of the language
module Syntax where

data Program = Declaration String Expression
             | EndProgram

type Name = String

-- Lambda Expressions need a mechanism to handle naming clashes when replacing
-- variables (bound variables with mathcing names should be renamed
data Expression = Lambda Name Expression
                | Name
                | Application Expression Expression
-- Non-Lambda Expressions:
                | Literal Int
                | Arithmetic Expression Op Expression
                | IfThenElse Expression Expression Expression

data Op = Add | Sub | Mul | Div | Mod

eval :: Expression -> Int
eval (Application (Lambda n x) y
eval (Arithmetic x Add y) = (eval x) + (eval y)
eval (Arithmetic x Sub y) = (eval x) - (eval y)
eval (Arithmetic x Mul y) = (eval x) * (eval y)
eval (Arithmetic x Div y) = div (eval x) (eval y)
eval (Arithmetic x Mod y) = mod (eval x) (eval y)
eval (IfThenElse i t e)   = if (eval i) /= 0 then (eval t) else (eval e)
eval (Literal x)          = x
