module Interpreter where

import AbstractSyntax

-- REPLACEMENT ALGORITHM
-- Create a list of unbound names in the argument expression
-- For each name in that list
--     call newName to get a name that doesn't exist in function body
--     call renameBound newName functionBody to do the replacement
-- Finally carry out the substitution

-- Reduces an Expression to its normal form
reduce :: Expression -> Expression
reduce (Application (Lambda n x) y) = replace n (preventClashes x y) y
    where preventClashes x y = renameAll (freeNames y) x

reduce (Arithmetic (Literal x) op (Literal y)) = Literal valueXY
    where valueXY = eval (Arithmetic (Literal x) op (Literal y))

-- Evaluates an Expression, and returns its value. Expressions with remaining
-- abstractions, appplications and names, are not handled.
eval :: Expression -> Int
eval (Arithmetic x op y) = doOp op (eval x) (eval y)
eval (IfThenElse i t e)  = if (eval i) /= 0 then (eval t) else (eval e)
eval (Literal x)         = x
eval _                   = error "Cannot calculate value of a lambda term"

-- Function to replace one Expression with another - implements function
-- application with Expressions. Assumes that there are no name clashes.
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
