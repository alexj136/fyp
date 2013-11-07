-- This module defines the abstract syntax of the language
module AbstractSyntax where

import qualified Data.Set as Set

-- Lambda Expressions need a mechanism to handle naming clashes when replacing
-- variables (bound variables with mathcing names should be renamed
data Expression = Abs Name Expression
                | Var Name
                | App Expression Expression
-- Churchy Expressions:
                | Literal Int
                | Arithmetic Expression Op Expression
                | IfThenElse Expression Expression Expression
    deriving (Show, Eq)

type Name = String

data Op = Add | Sub | Mul | Div | Mod
    deriving (Show, Eq)

-- Retrieve the function that corresponds to the given Op
getOp :: (Integral a, Num a) => Op -> (a -> a -> a)
getOp Add = (+)
getOp Sub = (-)
getOp Mul = (*)
getOp Div = div
getOp Mod = mod

-- Returns a list of the subexpressions of the given tree node
subs :: Expression -> [Expression]
subs (Abs _ x)          = [x]
subs (App x y)          = [x, y]
subs (Arithmetic x _ y) = [x, y]
subs (IfThenElse x y z) = [x, y, z]
subs _                  = []

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
(===) (Literal x)         (Literal y)          = x == y
(===) (Var n)             (Var n')             = n == n'
(===) (Abs n x)           (Abs n' y)           = renamedX == renamedY
    where renamedX = renameAll allNames (Abs n  x)
          renamedY = renameAll allNames (Abs n' y)
          allNames = Set.toList $ Set.fromList $ names x ++ names y ++ [n, n']
(===) (App x y)           (App a b)            = x === a && y === b
(===) (Arithmetic x op y) (Arithmetic a op' b) = x === a && op == op' && y === b
(===) (IfThenElse x y z)  (IfThenElse a b c)   = x === a && y === b   && z === c
(===) _                   _                    = False
--}

-- Creates a list of all name occurences in an Expression - If a name is used
-- twice, it will appear twice in the returned list, etc.
names :: Expression -> [Name]
names (Abs n x)   = n : names x
names (Var n)     = [n]
names (Literal _) = []
names exp         = concat (map names (subs exp))

-- Returns true if there are any occurences (free or bound) of the given string
-- as a name in the given Expression. Used by newName to generate names that are
-- not found in a given expression.
nameIn :: Name -> Expression -> Bool
nameIn n (Abs n' x)  = (n == n') || nameIn n x
nameIn n (Var n')    = n == n'
nameIn _ (Literal _) = False
nameIn n exp         = any (nameIn n) (subs exp)

-- Creates a set of all free variables in an Expression, which can be used when
-- reducing a function application, as it determines which names will need to be
-- changed in order to prevent name clashes.
freeVars :: Expression -> Set.Set Name
freeVars exp = case exp of
    Var x     -> Set.singleton x
    Abs x m   -> Set.delete x (freeVars m)
    Literal _ -> Set.empty
    _         -> Set.unions $ map freeVars $ subs exp

-- Composes Set.toList with freeVars, yeilding a list of the free names in an
-- Expression, as opposed to a Set
freeNames :: Expression -> [Name]
freeNames = Set.toList . freeVars

-- Returns a string which does not exist within a given set of names. Begins by
-- choosing the name "x", and if that already exists in the given set, add a
-- prime (') character to it. Keep adding prime characters until we have a name
-- for which the nameIn function will return false.
newName :: Set.Set Name -> Name
newName set | Set.member "x" set = newName2 "x" set
               | otherwise          = "x"
    where newName2 lastTry set
              | Set.member lastTry' set = newName2 lastTry' set
              | otherwise               = lastTry'
                  where lastTry' = lastTry ++ "'"

-- Rename every occurence in the given Expression, of every name in the given
-- list, such that no name clashes can occur if Expressions containing names in
-- the given list are substituted into the given Expression.
renameAll :: [Name] -> Expression -> Expression
renameAll = renameAll2 []

renameAll2 :: [Name] -> [Name] -> Expression -> Expression
renameAll2 _    []   x = x
renameAll2 done (h:t) x = renameAll2 (h:done) t (renameBound h freshName x)
    where freshName = newName (Set.fromList ((names x) ++ done ++ [h] ++ t))

-- Rename only the bound instances of the given name in the given expression.
-- Recurse down the tree until we find the given binding, at which point we can
-- do a 'blind' rename of every instance of that name that we find, as they are
-- guaranteed to be bound, either by the binding we just found, or by a another
-- binding of the same name. In both cases, we want to rename, because either
-- could potentially clash with a substituted expression. If we find the name
-- before we find the binding, we don't need to do anything - the name is not
-- bound at that point and therefore doesn't need renaming.
renameBound :: Name -> Name -> Expression -> Expression
renameBound _    _  (Literal x) = Literal x
renameBound _    _  (Var n)     = Var n
renameBound from to exp         = case exp of
    Abs n x | n == from -> Abs to (blindRename from to x)
            | otherwise -> Abs n (renamed x)
    App x x'            -> App (renamed x) (renamed x')
    Arithmetic x op x'  -> Arithmetic (renamed x) op (renamed x')
    IfThenElse x x' x'' -> IfThenElse (renamed x) (renamed x') (renamed x'')
    where renamed = renameBound from to

-- Renames every name with value 'from' to value 'to' in an Expression, with no
-- regard for clashes. It is assumed that this function will only be called on
-- Expressions where the potential for incorrectly renaming unbound names has
-- been eliminated.
blindRename :: Name -> Name -> Expression -> Expression
blindRename from to exp = case exp of
    Abs n x | n == from -> Abs to (renamed x)
            | otherwise -> Abs n (renamed x)
    Var n   | n == from -> Var to
            | otherwise -> Var n
    App x x'            -> App (renamed x) (renamed x')
    Arithmetic x op x'  -> Arithmetic (renamed x) op (renamed x')
    IfThenElse x x' x'' -> IfThenElse (renamed x) (renamed x') (renamed x'')
    Literal x           -> Literal x
    where renamed = blindRename from to
