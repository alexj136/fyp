module Interpreter where

import qualified Data.Set as Set
import AbstractSyntax

-- REPLACEMENT ALGORITHM
-- Create a list of unbound names in the argument expression
-- For each name in that list
--     call newName to get a name that doesn't exist in function body
--     call renameBound newName functionBody to do the replacement
-- Finally carry out the substitution

-- Function for convenience in the REPL, shorthand for: reduce (Application x y)
apply :: Expression -> Expression -> Expression
apply x y = reduceNorm (App x y)

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
eval (Constant (CInt x)) = x
eval _                   = error "Cannot calculate value of ill-typed expression"

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
    Constant x          -> Constant x
    where replaced subBodyExp = replace n subBodyExp argExp

-- The === operator checks if two expressions are α-equivalent, i.e. are they
-- are equal disregarding names. Some examples:
-- \x.x   === \y.y   -> True
-- \xy.yx === \sz.zs -> True
-- \ab.ac === \xy.yz -> False
(===) :: Expression -> Expression -> Bool
(===) x y = renamedX == renamedY
    where renamedX = renameAll allNames x
          renamedY = renameAll allNames y
          allNames = Set.toList $ Set.fromList $ names x ++ names y

{-- PREVIOUS IMPLEMENTATION - Closer to the definition of α-equivalence, but in
    practice more susceptible to name clash problems.
(===) (Constant x)        (Constant y)         = x == y
(===) (Var n)             (Var n')             = n == n'
(===) (Abs n x)           (Abs n' y)           = renamedX == renamedY
    where renamedX = renameAll allNames (Abs n  x)
          renamedY = renameAll allNames (Abs n' y)
          allNames = Set.toList $ Set.fromList $ names x ++ names y ++ [n, n']
(===) (App x y)           (App a b)            = x === a && y === b
(===) (Arithmetic x op y) (Arithmetic a op' b) = x === a && op == op' && y === b
(===) _                   _                    = False
--}

-- Returns a string which does not exist within a given set of names. Begins by
-- choosing the name "x", and if that already exists in the given set, add a
-- prime (') character to it. Keep adding prime characters until we have a name
-- for which the nameIn function will return false.
newName :: Set.Set Name -> Name
newName set | Set.member "x" set = newName2 "x" set
            | otherwise          = "x"
    where newName2 :: Name -> Set.Set Name -> Name
          newName2 lastTry set
              | Set.member lastTry' set = newName2 lastTry' set
              | otherwise               = lastTry'
                  where lastTry' :: String
                        lastTry' = lastTry ++ "'"

-- Rename every occurence in the given Expression, of every name in the given
-- list, such that no name clashes can occur if Expressions containing names in
-- the given list are substituted into the given Expression.
renameAll :: [Name] -> Expression -> Expression
renameAll = renameAll2 []

renameAll2 :: [Name] -> [Name] -> Expression -> Expression
renameAll2 _    []   x = x
renameAll2 done (h:t) x = renameAll2 (h:done) t (renameBound h freshName x)
    where freshName :: Name
          freshName = newName (Set.fromList ((names x) ++ done ++ [h] ++ t))

-- Rename only the bound instances of the given name in the given expression.
-- Recurse down the tree until we find the given binding, at which point we can
-- do a 'blind' rename of every instance of that name that we find, as they are
-- guaranteed to be bound, either by the binding we just found, or by a another
-- binding of the same name. In both cases, we want to rename, because either
-- could potentially clash with a substituted expression. If we find the name
-- before we find the binding, we don't need to do anything - the name is not
-- bound at that point and therefore doesn't need renaming.
renameBound :: Name -> Name -> Expression -> Expression
renameBound _    _  (Constant x) = Constant x
renameBound _    _  (Var n)      = Var n
renameBound from to exp          = case exp of
    Abs n x | n == from -> Abs to (blindRename from to x)
            | otherwise -> Abs n (renamed x)
    App x x'            -> App (renamed x) (renamed x')
    Arithmetic x op x'  -> Arithmetic (renamed x) op (renamed x')
    where renamed :: Expression -> Expression
          renamed = renameBound from to

-- Renames every name with value 'from' to value 'to' in an Expression, with no
-- regard for clashes. It is assumed that this function will only be called on
-- Expressions where the potential for incorrectly renaming unbound names has
-- been eliminated.
blindRename :: Name -> Name -> Expression -> Expression
blindRename from to exp = case exp of
    Abs n x | n == from  -> Abs to (renamed x)
            | otherwise  -> Abs n (renamed x)
    Var n   | n == from  -> Var to
            | otherwise  -> Var n
    App x x'             -> App (renamed x) (renamed x')
    Arithmetic x op x'   -> Arithmetic (renamed x) op (renamed x')
    Constant x           -> Constant x
    where renamed :: Expression -> Expression
          renamed = blindRename from to
