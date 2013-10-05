module Interpreter where

import qualified Data.Set as Set
import AbstractSyntax

-- Reduces an Expression to its normal form
reduce :: Expression -> Expression
reduce (Application (Lambda n x) y) = replace n (preventClashes x y) y
    where preventClashes x y = renameAll (Set.toList (getNames Set.empty Set.empty y)) x

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

-- Creates a set of all unbound names in an Expression, which can be used when
-- reducing a function application, as it determines which names will need to be
-- changed in order to prevent name clashes.
--     The function walks the AST, keeping a set of free names, and a set of
-- bound names. When a name is encountered as a variable (i.e. not as a Lambda
-- abstraction) it is added to the free set, but only if it does not already
-- appear in the bound set, which would indicate that that name is already
-- bound. When a name is encountered in a Lambda abstraction, it is added to the
-- bound set, so that all further instances of that name are ignored, because
-- they are bound in this abstraction.
getNames :: Set.Set String -> Set.Set String -> Expression -> Set.Set String
getNames free bound (Name n)              | Set.member n bound = free
                                          | otherwise          = Set.insert n free
getNames free bound (Lambda n x)          = getNames free (Set.insert n bound) x
getNames free bound (Application x x')    = Set.unions (map (getNames free bound) [x, x'])
getNames free bound (Arithmetic x _ x')   = Set.unions (map (getNames free bound) [x, x'])
getNames free bound (IfThenElse x x' x'') = Set.unions (map (getNames free bound) [x, x', x''])
getNames free bound _                     = free

-- Returns a string which does not exist as a name within a given Expression.
-- Begins by choosing the name "x", and if that already exists in the given
-- expression, add a prime (') character to it. Keep adding prime characters
-- until we have a name for which the nameIn function will return false.
newName :: Expression -> String
newName x = if nameIn "x" x then newName' "x" x else "x"
    where newName' lastTry x = if nameIn (lastTry ++ "'") x
                               then newName' (lastTry ++ "'") x
                               else lastTry ++ "'"

-- Returns true if there are any occurences (free or bound) of the given string
-- as a name in the given Expression. Used by newName to generate names that are
-- not found in a given expression.
nameIn :: String -> Expression -> Bool
nameIn n (Lambda n' x)         = (n == n') || nameIn n x
nameIn n (Name n')             = n == n'
nameIn n (Application x x')    = any (nameIn n) [x, x']
nameIn n (Arithmetic x _ x')   = any (nameIn n) [x, x']
nameIn n (IfThenElse x x' x'') = any (nameIn n) [x, x', x'']
nameIn _ (Literal _)           = False
