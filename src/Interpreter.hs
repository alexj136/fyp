module Interpreter where

import qualified Data.Set as Set
import AbstractSyntax

-- Reduces an Expression to its normal form
reduce :: Expression -> Expression
reduce (Application (Lambda n x) y) = replace n (preventClashes x y) y
    where preventClashes x y = renameAll (Set.toList (freeNames Set.empty Set.empty y)) x

reduce (Arithmetic (Literal x) op (Literal y)) = Literal valueXY
    where valueXY = eval (Arithmetic (Literal x) op (Literal y))

-- Evaluates an Expression, and returns its value
eval :: Expression -> Int
eval (Arithmetic x Add y) = (eval x) + (eval y)
eval (Arithmetic x Sub y) = (eval x) - (eval y)
eval (Arithmetic x Mul y) = (eval x) * (eval y)
eval (Arithmetic x Div y) = div (eval x) (eval y)
eval (Arithmetic x Mod y) = mod (eval x) (eval y)
eval (IfThenElse i t e)   = if (eval i) /= 0 then (eval t) else (eval e)
eval (Literal x)          = x

-- REPLACEMENT ALGORITHM
-- Create a list of unbound names in the argument expression
-- For each name in that list
--     call newName to get a name that doesn't exist in function body
--     call renameBound newName functionBody to do the replacement
-- Finally carry out the substitution

-- Function to replace one Expression with another - implements function
-- application with Expressions. Assumes that there are no name clashes.
replace :: String -> Expression -> Expression -> Expression
replace n (Lambda n' x) y         = Lambda n' (replace n x y)
replace n (Application x x') y    = Application (replace n x y) (replace n x' y)
replace n (Name n') y             | n == n'   = y
                                  | otherwise = (Name n')
replace n (Arithmetic x op x') y  = Arithmetic (replace n x y) op (replace n x' y)
replace n (IfThenElse x x' x'') y = IfThenElse (replace n x y) (replace n x' y) (replace n x'' y)
replace _ (Literal x) _           = Literal x

-- Rename every occurence in the given Expression, of every name in the given
-- list, such that no name clashes can occur if Expressions containing names in
-- the given list are substituted into the given Expression.
renameAll :: [String] -> Expression -> Expression
renameAll [] x = x
renameAll ns x = renameAll (tail ns) nextX
    where nextX = renameBound (head ns) (newName x) x

-- Rename only the bound instances of the given name in the given expression.
-- Recurse down the tree until we find the given binding, at which point we can
-- do a 'blind' rename of every instance of that name that we find, as they are
-- guaranteed to be bound by the binding we just found. If we find the name
-- before we find the binding, we don't need to do anything - the name is not
-- bound at that point and therefore doesn't need renaming.
renameBound :: String -> String -> Expression -> Expression
renameBound from to (Lambda n x)          | n == from = Lambda to (blindRename from to x)
                                          | otherwise = Lambda n (renameBound from to x)
renameBound from to (Application x x')    = Application (renameBound from to x) (renameBound from to x')
renameBound from to (Arithmetic x op x')  = Arithmetic (renameBound from to x) op (renameBound from to x')
renameBound from to (IfThenElse x x' x'') = IfThenElse (rB x) (rB x') (rB x'')
    where rB = renameBound from to
renameBound _    _  x                     = x

-- Renames every name with value 'from' to value 'to' in an Expression, with no
-- regard for clashes. It is assumed that this function will only be called on
-- Expressions where the potential for incorrectly renaming unbound names has
-- been eliminated.
blindRename :: String -> String -> Expression -> Expression
blindRename from to (Lambda n x)          | n == from = Lambda to (blindRename from to x)
                                          | otherwise = Lambda n (blindRename from to x)
blindRename from to (Application x x')    = Application (blindRename from to x) (blindRename from to x')
blindRename from to (Name n)              = if n == from then (Name to) else (Name n)
blindRename from to (Arithmetic x op x')  = Arithmetic (blindRename from to x) op (blindRename from to x')
blindRename from to (IfThenElse x x' x'') = IfThenElse (bR x) (bR x') (bR x'')
    where bR = blindRename from to
blindRename _    _  (Literal x)           = Literal x
