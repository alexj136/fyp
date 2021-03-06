module TypedInterpreter where

import qualified Data.Set as Set
import TypedSyntax

-- REPLACEMENT ALGORITHM
-- Create a list of unbound names in the argument expression
-- For each name in that list
--     call newName to get a name that doesn't exist in function body
--     call renameBound newName functionBody to do the replacement
-- Finally carry out the substitution

-- Function for convenience in the REPL, shorthand for: reduce (App x y)
apply :: TypedExp -> TypedExp -> TypedExp
apply x y = reduceNorm (App x y)

-- Performs at least one reduction step
reduce :: TypedExp -> TypedExp
reduce exp = case exp of
    App (App (BinaryOp op) m) n -> case ( op , redM , redN ) of
        ( Add , Constant (IntVal  x) , Constant (IntVal  y) ) -> Constant (IntVal  ((+) x y))
        ( Sub , Constant (IntVal  x) , Constant (IntVal  y) ) -> Constant (IntVal  ((-) x y))
        ( Mul , Constant (IntVal  x) , Constant (IntVal  y) ) -> Constant (IntVal  ((*) x y))
        ( Div , Constant (IntVal  x) , Constant (IntVal  y) ) -> Constant (IntVal  (div x y))
        ( Mod , Constant (IntVal  x) , Constant (IntVal  y) ) -> Constant (IntVal  (mod x y))
        ( Xor , Constant (BoolVal x) , Constant (BoolVal y) ) -> Constant (BoolVal (xor x y))
        ( And , Constant (BoolVal x) , Constant (BoolVal y) ) -> Constant (BoolVal (x && y))
        ( Or  , Constant (BoolVal x) , Constant (BoolVal y) ) -> Constant (BoolVal (x || y))
        where redM, redN :: TypedExp
              redM = reduce m
              redN = reduce n
              xor :: Bool -> Bool -> Bool
              xor a b = case ( a , b ) of
                  ( True  , True  ) -> False
                  ( True  , False ) -> True
                  ( False , True  ) -> True
                  ( False , False ) -> False
    App (UnaryOp op) m -> case ( op , redM ) of
        ( IsZ , Constant (IntVal  x) ) -> Constant (BoolVal (x == 0))
        ( Not , Constant (BoolVal x) ) -> Constant (BoolVal (not x))
        where redM = reduce m
    Abs v t m         -> Abs v t (reduce m)
    App (Abs v t m) n -> replace v (preventClashes m n) n
    App m           n -> case m of
        Var      _ -> App m (reduce n)
        Constant _ -> App m (reduce n)
        _             -> App (reduce m) n
    _                 -> exp

-- Keep reducing an expression until it stops changing i.e. until it reaches
-- normal form
reduceNorm :: TypedExp -> TypedExp
reduceNorm exp = if exp == reducedExp then exp else reduceNorm reducedExp
    where reducedExp :: TypedExp
          reducedExp = reduce exp

-- Given two expressions, M and N, derive the expression M' where M' and M are
-- α-equivalent, but the bound variable names in M have been changes such that
-- they cannot possibly clash with names in N should N be applied to M
preventClashes :: TypedExp -> TypedExp -> TypedExp
preventClashes x y = renameAll (freeNames y) x

-- Function to replace one expression with another - implements function
-- application with expressions. Assumes that there are no name clashes.
replace :: Name -> TypedExp -> TypedExp -> TypedExp
replace varName bodyExp argExp = case bodyExp of
    Abs v t m | varName /= v -> Abs v t (replaced m)
              | varName == v -> Abs v t m
    App m n                  -> App (replaced m) (replaced n)
    Var v     | varName == v -> argExp
              | varName /= v -> Var v
    where replaced subBodyExp = replace varName subBodyExp argExp

-- The === operator checks if two expressions are α-equivalent, i.e. are they
-- are equal disregarding names. Some examples:
-- \x.x   === \y.y   -> True
-- \xy.yx === \sz.zs -> True
-- \ab.ac === \xy.yz -> False
(===) :: TypedExp -> TypedExp -> Bool
(===) x y = renamedX == renamedY
    where renamedX = renameAll allNames x
          renamedY = renameAll allNames y
          allNames = Set.toList $ Set.fromList $ names x ++ names y

{-- PREVIOUS IMPLEMENTATION - Closer to the definition of α-equivalence, but
    suffers from the name clash problem.
(===) (Constant x)        (Constant y)         = x == y
(===) (Var n)             (Var n')             = n == n'
(===) (Abs n x)           (Abs n' y)           = x === renamedY
    where renamedY = renameAll
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

-- Rename every occurence in the given expression, of every name in the given
-- list, such that no name clashes can occur if expressions containing names in
-- the given list are substituted into the given expression.
renameAll :: [Name] -> TypedExp -> TypedExp
renameAll = rnmAll2 []
    where rnmAll2 :: [Name] -> [Name] -> TypedExp -> TypedExp
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
renameBound :: Name -> Name -> TypedExp -> TypedExp
renameBound from to exp          = case exp of
    Abs v t m | v == from -> Abs to t (blindRename from to m)
              | otherwise -> Abs v  t (renamed m)
    App m n               -> App (renamed m) (renamed n)
    _                     -> exp
    where renamed :: TypedExp -> TypedExp
          renamed = renameBound from to

-- Renames every name with value 'from' to value 'to' in an expression, with no
-- regard for clashes. It is assumed that this function will only be called on
-- expressions where the potential for incorrectly renaming unbound names has
-- been eliminated.
blindRename :: Name -> Name -> TypedExp -> TypedExp
blindRename from to exp = case exp of
    Abs v t m | v == from -> Abs to t (renamed m)
              | otherwise -> Abs v  t (renamed m)
    Var v     | v == from -> Var to
              | otherwise -> Var v
    App m n               -> App (renamed m) (renamed n)
    _                     -> exp
    where renamed :: TypedExp -> TypedExp
          renamed = blindRename from to
