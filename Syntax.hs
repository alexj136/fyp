-- This module defines the abstract syntax of the language
module Syntax where

import qualified Data.Set as Set

data Program = Declaration String Expression
             | EndProgram

-- Lambda Expressions need a mechanism to handle naming clashes when replacing
-- variables (bound variables with mathcing names should be renamed
data Expression = Lambda String Expression
                | Name String
                | Application Expression Expression
-- Churchy Expressions:
                | Literal Int
                | Arithmetic Expression Op Expression
                | IfThenElse Expression Expression Expression
    deriving Show

data Op = Add | Sub | Mul | Div | Mod
    deriving Show

-- Function to replace one Expression with another - implements function
-- application with Expressions. Assumes that there are no name clashes.
replace :: Expression -> Expression -> Expression -> Expression
replace (Name n) (Lambda n' x) y         = Lambda n' (replace (Name n) x y) 
replace (Name n) (Application x x') y    = Application (replace (Name n) x y) (replace (Name n) x' y)
replace (Name n) (Name n') y             | n == n'   = y
                                         | otherwise = (Name n')
replace (Name n) (Arithmetic x op x') y  = Arithmetic (replace (Name n) x y) op (replace (Name n) x' y)
replace (Name n) (IfThenElse x x' x'') y = IfThenElse (replace (Name n) x y) (replace (Name n) x' y) (replace (Name n) x'' y)
replace _ (Literal x) _                  = Literal x

-- Renames all bound occurences in the given Expression of any of the specified
-- names
rename :: [String] -> Expression -> Expression
rename [] x = x
rename ns (Lambda n x) | any (== n) ns =
rename ns (Name n)     | any (== n) ns = Name (n ++ "'")
                       |

-- FINAL REPLACEMENT ALGORITHM
-- Create a list of unbound names in the argument expression
-- For each name in that list
--     call newName to get a name that doesn't exist in function body
--     call renameBound newName functionBody to do the replacement
-- Finally carry out the substitution

-- Rename ONLY BOUND instances of the given name in the given expression
-- recurse down the tree until we find the given binding, at which point we can
-- do a careless rename of every instance of that name that we find. If we find
-- the name before we find the binding, we don't need to do anything - the name
-- is not bound at that point and therefore doesn't need renaming.
renameBound :: String -> String -> Expression -> Expression
renameBound from to (Lambda n x)          | n == from = Lambda to (blindRename from to x)
                                          | otherwise = Lambda n (renameBound from to x)
renameBound from to (Application x x')    = Application (renameBound from to x) (renameBound from to x')
renameBound from to (Arithemtic x op x')  = Arithmetic (renameBound from to x) op (renameBound from to x')
renameBound from to (IfThenElse x x' x'') = IfThenElse (rB x) (rB x') (rB x'') where rB = renameBound from to
renameBound _    _  x                     = x

-- Returns a string which does not exist as a name within a given Expression
newName :: Expression -> String
newName _ = error "newName not yet implemented"

-- Renames every name with value 'from' to value 'to', with no regard for
-- clashes etc
blindRename :: String -> String -> Expression -> Expression
blindRename from to (Lambda n x)          | n == from = Lambda to (blindRename n x)
                                          | otherwise = Lambda n (blindRename n x)
blindRename from to (Application x x')    = Application (blindRename from to x) (blindRename from to x')
blindRename from to (Name n)              = if n == from then (Name to) else (Name n)
blindRename from to (Arithmetic x op x')  = Arithmetic (blindRename from to x) op (blindRename from to x')
blindRename from to (IfThenElse x x' x'') = IfThenElse (bR x) (bR x') (bR x'') where bR = blindRename from to
blindRename _    _  (Literal x)           = Literal x

-- Returns true if there are ANY occurences of the given string as a name in
-- the given Expression
nameIn :: String -> Expression -> Bool
nameIn n (Lambda n' x)         = (n == n') || nameIn n x
nameIn n (Name n')             = n == n'
nameIn n (Application x x')    = any (nameIn n) [x, x']
nameIn n (Arithmetic x _ x')   = any (nameIn n) [x, x']
nameIn n (IfThenElse x x' x'') = any (nameIn n) [x, x', x'']
nameIn _ (Literal _)           = False

-- Returns true unless there are free occurences of the given name in the given
-- Expression
boundIn :: String -> Expression -> Bool
boundIn n (Lambda n' x)         | n == n'   = True
                                | otherwise = boundIn (Name n) x
boundIn n (Application x x')    = all (boundIn (Name n)) [x, x']
boundIn n (Name n')             = n /= n'
boundIn n (Arithmetic x _ x')   = all (boundIn (Name n)) [x, x']
boundIn n (IfThenElse x x' x'') = all (boundIn (Name n)) [x, x', x'']
boundIn n (Literal _)           = True

-- Creates a set of all unbound names in an Expression
getNames :: Set.Set String -> Set.Set String -> Expression -> Set.Set String
getNames free bound (Name n)              | Set.member n bound = free
                                          | otherwise          = Set.insert n free
getNames free bound (Lambda n x)          = getNames free (Set.insert n bound) x
getNames free bound (Application x x')    = Set.unions (map (getNames free bound) [x, x'])
getNames free bound (Arithmetic x _ x')   = Set.unions (map (getNames free bound) [x, x'])
getNames free bound (IfThenElse x x' x'') = Set.unions (map (getNames free bound) [x, x', x''])
getNames free bound _                     = free

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
