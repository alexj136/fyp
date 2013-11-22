module PureSyntax where

import qualified Data.Set as S

type Name = String

-- The three components of the λ-calculus
data Expression = Abs Name Expression
                | Var Name
                | App Expression Expression
    deriving Eq

instance Show Expression where
    show exp = case exp of
        Abs v (App m n)      -> 'λ':v ++ '.':show m ++ ' ':show n
        Abs v m              -> 'λ':v ++ '.':show m
        Var v                -> v
        App m n              -> '(':show m ++ ' ':show n ++ ")"

-- Returns a list of the subexpressions of the given tree node
subs :: Expression -> [Expression]
subs (Abs _ m) = [m]
subs (App m n) = [m, n]
subs (Var _)   = []

-- Creates a list of all name occurences in an Expression - If a name is used
-- twice, it will appear twice in the returned list, etc.
names :: Expression -> [Name]
names (Abs v m) = v : names m
names (Var v)   = [v]
names (App m n) = names m ++ names n

-- Returns true if there are any occurences (free or bound) of the given string
-- as a name in the given Expression. Used by newName to generate names that are
-- not found in a given expression.
nameIn :: Name -> Expression -> Bool
nameIn v (Abs v' m) = (v == v') || nameIn v m
nameIn v (Var v')   = v == v'
nameIn v (App m n)  = nameIn v m || nameIn v n

-- Creates a set of all free variables in an Expression, which can be used when
-- reducing a function application, as it determines which names will need to be
-- changed in order to prevent name clashes.
freeVars :: Expression -> S.Set Name
freeVars exp = case exp of
    Var v   -> S.singleton v
    Abs v m -> S.delete v (freeVars m)
    App m n -> S.union (freeVars m) (freeVars n)

-- Composes S.toList with freeVars, yeilding a list of the free names in an
-- Expression, as opposed to a Set
freeNames :: Expression -> [Name]
freeNames = S.toList . freeVars
