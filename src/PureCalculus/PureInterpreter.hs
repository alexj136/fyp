module PureInterpreter where

import qualified Data.Set as Set
import PureSyntax

-- Function for convenience in the REPL, shorthand for: reduce (App m n)
apply :: Expression -> Expression -> Expression
apply m n = reduceNorm (App m n)

-- Applies a list of expressions to one another, for example,
-- 'applyAll [M, N, O, P]' is equivalent to reduceNorm (((M N) O) P)
applyAll :: [Expression] -> Expression
applyAll (m:n:rest) = foldl apply m (n:rest)
applyAll _          = error "Two expressions required by applyAll"

-- Performs at least one reduction step
reduce :: Expression -> Expression
reduce exp = case exp of
    Abs v m         -> Abs v (reduce m)
    App (Abs v m) n -> replace v (preventClashes m n) n
    App m         n | redM === m -> App m (reduce n) -- only reduce rhs when lhs
                    | otherwise  -> App (reduce m) n -- is in normal form
        where redM = reduce m
    _               -> exp

-- Keep reducing an expression until it stops changing i.e. until it reaches
-- normal form
reduceNorm :: Expression -> Expression
reduceNorm exp = if exp == reducedExp then exp else reduceNorm reducedExp
    where reducedExp :: Expression
          reducedExp = reduce exp

-- Given two expressions, M and N, derive the expression M' where M' and M are
-- α-equivalent, but the bound variable names in M have been changes such that
-- they cannot possibly clash with names in N should N be applied to M
preventClashes :: Expression -> Expression -> Expression
preventClashes x y = renameAll (freeNames y) x

-- Function to replace one Expression with another - implements function
-- application with Expressions. Assumes that there are no name clashes.
replace :: Name -> Expression -> Expression -> Expression
replace v bodyExp argExp = case bodyExp of
    Abs v' m | v /= v' -> Abs v' (replaced m)
             | v == v' -> Abs v' m
    App m  n           -> App (replaced m) (replaced n)
    Var v'   | v == v' -> argExp
             | v /= v' -> Var v'
    where replaced subBodyExp = replace v subBodyExp argExp

-- The === operator checks if two expressions are α-equivalent, i.e. are they
-- are equal disregarding names. Some examples:
-- \x.x   === \y.y   -> True
-- \xy.yx === \sz.zs -> True
-- \ab.ac === \xy.yz -> False
(===) :: Expression -> Expression -> Bool
(===) m n = rename m == rename n
    where rename = renameAll allNames
          allNames = Set.toList $ Set.fromList $ names m ++ names n

{-- PREVIOUS IMPLEMENTATION - Closer to the definition of α-equivalence, but
    suffers from the name clash problem.
(===) (Var n)   (Var n')   = n == n'
(===) (Abs n x) (Abs n' y) = x === renamedY
    where renamedY = renameAll
(===) (App x y) (App a b)  = x === a && y === b
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
renameAll = rnmAll2 []
    where rnmAll2 :: [Name] -> [Name] -> Expression -> Expression
          rnmAll2 done todo x = case todo of
              []  -> x
              h:t -> rnmAll2 (h:done) t (renameBound h fresh x)
              where fresh :: Name
                    fresh = newName $ Set.fromList ((names x) ++ done ++ todo)

-- Rename only the bound instances of the given name in the given expression.
-- Recurse down the tree until we find the given binding, at which point we can
-- do a 'blind' rename of every instance of that name that we find, as they are
-- guaranteed to be bound, either by the binding we just found, or by a another
-- binding of the same name. In both cases, we want to rename, because either
-- could potentially clash with a substituted expression. If we find the name
-- before we find the binding, we don't need to do anything - the name is not
-- bound at that point and therefore doesn't need renaming.
renameBound :: Name -> Name -> Expression -> Expression
renameBound from to exp = case exp of
    Abs v m | v == from -> Abs to (blindRename from to m)
            | otherwise -> Abs v (renamed m)
    App m n             -> App (renamed m) (renamed n)
    Var _               -> exp
    where renamed :: Expression -> Expression
          renamed = renameBound from to

-- Renames every name with value 'from' to value 'to' in an Expression, with no
-- regard for clashes. It is assumed that this function will only be called on
-- Expressions where the potential for incorrectly renaming unbound names has
-- been eliminated.
blindRename :: Name -> Name -> Expression -> Expression
blindRename from to exp = case exp of
    Abs v m | v == from -> Abs to (renamed m)
            | otherwise -> Abs v (renamed m)
    Var v   | v == from -> Var to
            | otherwise -> Var v
    App m n             -> App (renamed m) (renamed n)
    where renamed :: Expression -> Expression
          renamed = blindRename from to
