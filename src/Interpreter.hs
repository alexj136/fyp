module Interpreter where

import AbstractSyntax

-- REPLACEMENT ALGORITHM
-- Create a list of unbound names in the argument expression
-- For each name in that list
--     call newName to get a name that doesn't exist in function body
--     call renameBound newName functionBody to do the replacement
-- Finally carry out the substitution

-- Function for convenience in the REPL, shorthand for: reduce (Application x y)
apply :: Expression -> Expression -> Expression
apply x y = reduce (App x y)

-- Performs a single reduction step
reduce :: Expression -> Expression
reduce exp = case exp of
    App (Abs n x) y -> replace n (preventClashes x y) y
    App x         y -> App (reduce x) (reduce y)
    Abs n x         -> Abs n (reduce x)
    _               -> exp

-- Keep reducing an expression until it stops changing i.e. until it reaches
-- normal form
reduceNorm :: Expression -> Expression
reduceNorm exp = if exp == reducedExp then exp else reduceNorm reducedExp
    where reducedExp = reduce exp

-- Given two expressions, M and N, derive the expression M' where M' and M are
-- α-equivalent, but the bound variable names in M have been changes such that
-- they cannot possibly clash with names in N should N be applied to M
preventClashes :: Expression -> Expression -> Expression
preventClashes x y = renameAll (freeNames y) x

-- Evaluates an Expression, and returns its value. Expressions with remaining
-- abstractions, applications and names, are not handled.
eval :: Expression -> Int
eval (Arithmetic x op y) = getOp op (eval x) (eval y)
eval (IfThenElse i t e)  = if (eval i) /= 0 then (eval t) else (eval e)
eval (Literal x)         = x
eval _                   = error "Cannot calculate value of a lambda term"

-- Function to replace one Expression with another - implements function
-- application with Expressions. Assumes that there are no name clashes.
replace :: Name -> Expression -> Expression -> Expression
replace n bodyExp argExp = case bodyExp of
    Abs n' x | n /= n'  -> Abs n' (replaced x)
             | n == n'  -> Abs n' x
    App x x'            -> App (replaced x) (replaced x')
    Var n'   | n == n'  -> argExp
             | n /= n'  -> Var n'
    Arithmetic x op x'  -> Arithmetic (replaced x) op (replaced x')
    IfThenElse x x' x'' -> IfThenElse (replaced x) (replaced x') (replaced x'')
    Literal x           -> Literal x
    where replaced subBodyExp = replace n subBodyExp argExp
