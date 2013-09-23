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

replace :: Name -> Expression -> Expression -> Expression
replace n (Lambda n' x) y         = if n /= n' then replace n x y 
                                    else error "Name clashes not yet handled"
replace n (Application x x') y    = Application (replace n x y) (replace n x' y)
replace n Name y                  = y
replace n (Arithmetic x _ x') y   = Arithmetic (replace n x y) (replace n x' y)
replace n (IfThenElse x x' x'') y = IfThenElse (replace n x y) (replace n x' y) (replace n x'' y)
replace _ (Literal x) _           = Literal x

eval :: Expression -> Int
eval (Application (Lambda n x) y = eval (replace n x y)
-- More lambda cases to handle
eval (Arithmetic x Add y) = (eval x) + (eval y)
eval (Arithmetic x Sub y) = (eval x) - (eval y)
eval (Arithmetic x Mul y) = (eval x) * (eval y)
eval (Arithmetic x Div y) = div (eval x) (eval y)
eval (Arithmetic x Mod y) = mod (eval x) (eval y)
eval (IfThenElse i t e)   = if (eval i) /= 0 then (eval t) else (eval e)
eval (Literal x)          = x
