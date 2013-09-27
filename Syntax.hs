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
replace (Name n) (Lambda n' x) y         | n /= n'   = replace (Name n) x y 
                                         | otherwise = replace (Name n) (rename (Name n) (Lambda n x)) y
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
enumerateNames (Name n) strList               | any (== n) strList = strList
                                              | otherwise          = strList ++ [n]
enumerateNames (Lambda n x) strList           | any (== n) strList = enumerateNames x strList
                                              | otherwise          = enumerateNames x (strList ++ [n])
enumerateNames (Application x x') strList     = mergeAsSets (enumerateNames x strList) (enumerateNames x' strList)
enumerateNames (Arithmetic x _ x') strList    = mergeAsSets (enumerateNames x strList) (enumerateNames x' strList)
enumerateNames (IfThenElse x x' x'') strList  = mergeAsSets (enumerateNames x strList) (mergeAsSets (enumerateNames x' strList) (enumerateNames x'' strList))
enumerateNames _ strList                      = strList

-- Add an element to a list if it does not already exist in that list
addIfNot :: Eq a => a -> [a] -> [a]
addIfNot x xs
    | any (== x) xs = xs
    | otherwise     = [x] ++ xs

-- Scales addIfNot to handle merging two lists
mergeAsSets :: Eq a => [a] -> [a] -> [a]
mergeAsSets [] xs  = xs
mergeAsSets xs []  = xs
mergeAsSets xs xs' = mergeAsSets (tail xs) (addIfNot (head xs) xs')

-- Renames all instances of STR with STR' in an Expression, with no regard for
-- binding or name clashes
rename :: Expression -> Expression -> Expression
rename (Name n) (Name n')             | n == n'   = Name (n ++ "'")
                                      | otherwise = Name n'
rename (Name n) (Lambda n' x)         | n == n'   = Lambda (n ++ "'") (rename (Name n) x)
                                      | otherwise = Lambda n' (rename (Name n) x)
rename (Name n) (Application x x')    = Application (rename (Name n) x) (rename (Name n) x)
rename (Name n) (Arithmetic x op x')  = Arithmetic (rename (Name n) x) op (rename (Name n) x')
rename (Name n) (IfThenElse x x' x'') = IfThenElse (rename (Name n) x) (rename (Name n) x') (rename (Name n) x'')
rename _ (Literal x)                  = Literal x

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
