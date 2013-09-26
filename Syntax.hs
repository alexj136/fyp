-- This module defines the abstract syntax of the language
module Syntax where

data Program = Declaration String Expression
             | EndProgram

-- Lambda Expressions need a mechanism to handle naming clashes when replacing
-- variables (bound variables with mathcing names should be renamed
data Expression = Lambda String Expression
                | Name String
                | Application Expression Expression
-- Non-Lambda Expressions:
                | Literal Int
                | Arithmetic Expression Op Expression
                | IfThenElse Expression Expression Expression
    deriving Show

data Op = Add | Sub | Mul | Div | Mod
    deriving Show

-- Function to replace one Expression with another - implements function
-- application with Expressions
replace :: Expression -> Expression -> Expression -> Expression
replace (Name n) (Lambda n' x) y         = if n /= n' then replace (Name n) x y 
                                           else replace (Name n) (rename (Name n) Lambda n x) y
replace (Name n) (Application x x') y    = Application (replace (Name n) x y) (replace (Name n) x' y)
replace (Name n) (Name n') y             = y
replace (Name n) (Arithmetic x op x') y  = Arithmetic (replace (Name n) x y) op (replace (Name n) x' y)
replace (Name n) (IfThenElse x x' x'') y = IfThenElse (replace (Name n) x y) (replace (Name n) x' y) (replace (Name n) x'' y)
replace _ (Literal x) _                  = Literal x

-- Function that identifies the presence of a certain variable name within an
-- Expression
nameBoundIn :: Expression -> Expression -> Bool
nameBoundIn (Name n) (Name n')             = n == n'
nameBoundIn (Name n) (Lambda n' x)         = (n == n') || nameBoundIn (Name n) x
nameBoundIn (Name n) (Application x x')    = nameBoundIn (Name n) x || nameBoundIn (Name n) x'
nameBoundIn (Name n) (Arithmetic x _  x')  = nameBoundIn (Name n) x || nameBoundIn (Name n) x'
nameBoundIn (Name n) (IfThenElse x x' x'') = nameBoundIn (Name n) x || nameBoundIn (Name n) x' || nameBoundIn (Name n) x''
nameBoundIn _ (Literal _)                  = False

enumerateNames :: Expression -> [String] -> [String]
enumerateNames (Name n) StrList               = if any (== n) StrList then StrList
                                                else StrList ++ [n]
enumerateNames (Lambda n x) StrList           = if any (== n) StrList then enumerateNames x StrList
                                                else enumerateNames x (StrList ++ [n])
enumerateNames (Application x x') StrList     = enumerateNames ...

-- Renames all instances of STR with STR' in an Expression, with no regard for
-- binding or name clashes
rename :: Expression -> Expression -> Expression
rename (Name n) (Name n')              = if n == n' then Name (n ++ "'")
                                         else Name n'
rename (Name n) (Lambda n' x)          = if n == n' then Lambda (Name n ++ "'") (rename (Name n) x)
                                         else Lambda (Name n') (rename (Name n) x)
rename (Name n) (Application x x')     = Application (rename (Name n) x) (rename (Name n) x)
rename (Name n) (Arithmetic x op x')   = Arithmetic (rename (Name n) x) (rename (Name n) x')
rename (Name n) (IfThenElse x x' x'')  = IfThenElse (rename (Name n) x) (rename (Name n) x') (rename (Name n) x'')
rename _ (Literal x)                   = Literal x

-- Function to evaluate Expressions
eval :: Expression -> Int
eval (Application (Lambda n x) y) = eval (replace (Name n) x y)
-- More lambda cases to handle
eval (Arithmetic x Add y) = (eval x) + (eval y)
eval (Arithmetic x Sub y) = (eval x) - (eval y)
eval (Arithmetic x Mul y) = (eval x) * (eval y)
eval (Arithmetic x Div y) = div (eval x) (eval y)
eval (Arithmetic x Mod y) = mod (eval x) (eval y)
eval (IfThenElse i t e)   = if (eval i) /= 0 then (eval t) else (eval e)
eval (Literal x)          = x
