module Interpreter where

import AbstractSyntax

-- Reduces an Expression to its normal form
reduce :: Expression -> Expression
reduce (Application (Lambda n x) y) = replace n (preventClashes x y) y
    where preventClashes x y = renameAll (freeNames y) x

reduce (Arithmetic (Literal x) op (Literal y)) = Literal valueXY
    where valueXY = eval (Arithmetic (Literal x) op (Literal y))

-- Evaluates an Expression, and returns its value. Expressions with remaining
-- abstractions, appplications and names, are not handled.
eval :: Expression -> Int
eval (Arithmetic x Add y) = (eval x) + (eval y)
eval (Arithmetic x Sub y) = (eval x) - (eval y)
eval (Arithmetic x Mul y) = (eval x) * (eval y)
eval (Arithmetic x Div y) = div (eval x) (eval y)
eval (Arithmetic x Mod y) = mod (eval x) (eval y)
eval (IfThenElse i t e)   = if (eval i) /= 0 then (eval t) else (eval e)
eval (Literal x)          = x

-- Function to replace one Expression with another - implements function
-- application with Expressions. Assumes that there are no name clashes.
--     REPLACEMENT ALGORITHM
--     Create a list of unbound names in the argument expression
--     For each name in that list
--         call newName to get a name that doesn't exist in function body
--         call renameBound newName functionBody to do the replacement
--     Finally carry out the substitution
replace :: String -> Expression -> Expression -> Expression
replace n bodyExp argExp = case bodyExp of
    Lambda n' x         -> Lambda n' (replaced x)
    Application x x'    -> Application (replaced x) (replaced x')
    Name n' | n == n'   -> argExp
            | otherwise -> Name n'
    Arithmetic x op x'  -> Arithmetic (replaced x) op (replaced x')
    IfThenElse x x' x'' -> IfThenElse (replaced x) (replaced x') (replaced x'')
    Literal x           -> Literal x
    where replaced subBodyExp = replace n subBodyExp argExp

-- Rename every occurence in the given Expression, of every name in the given
-- list, such that no name clashes can occur if Expressions containing names in
-- the given list are substituted into the given Expression.
renameAll :: [String] -> Expression -> Expression
renameAll []    x = x
renameAll (h:t) x = renameAll t (renameBound h (newName x) x)

-- Rename only the bound instances of the given name in the given expression.
-- Recurse down the tree until we find the given binding, at which point we can
-- do a 'blind' rename of every instance of that name that we find, as they are
-- guaranteed to be bound by the binding we just found. If we find the name
-- before we find the binding, we don't need to do anything - the name is not
-- bound at that point and therefore doesn't need renaming.
renameBound :: String -> String -> Expression -> Expression
renameBound _    _  (Literal x) = Literal x
renameBound _    _  (Name n)    = Name n
renameBound from to exp         = case exp of
    Lambda n x | n == from -> Lambda to (blindRename from to x)
               | otherwise -> Lambda n (renamed x)
    Application x x'       -> Application (renamed x) (renamed x')
    Arithmetic x op x'     -> Arithmetic (renamed x) op (renamed x')
    IfThenElse x x' x''    -> IfThenElse (renamed x) (renamed x') (renamed x'')
    where renamed = renameBound from to

-- Renames every name with value 'from' to value 'to' in an Expression, with no
-- regard for clashes. It is assumed that this function will only be called on
-- Expressions where the potential for incorrectly renaming unbound names has
-- been eliminated.
blindRename :: String -> String -> Expression -> Expression
blindRename from to exp = case exp of
    Lambda n x | n == from -> Lambda to (renamed x)
               | otherwise -> Lambda n (renamed x)
    Name n     | n == from -> Name to
               | otherwise -> Name n
    Application x x'       -> Application (renamed x) (renamed x')
    Arithmetic x op x'     -> Arithmetic (renamed x) op (renamed x')
    IfThenElse x x' x''    -> IfThenElse (renamed x) (renamed x') (renamed x'')
    Literal x              -> Literal x
    where renamed = blindRename from to
