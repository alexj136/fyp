-- This module defines the abstract syntax of the language, and a few basic
-- functions over them, which retrieve information from them.
module PureSyntax where

import qualified Data.Set as Set

type Name = String

-- Lambda Expressions need a mechanism to handle naming clashes when replacing
-- variables (bound variables with mathcing names should be renamed
data Expression = Abs Name Expression
                | Var Name
                | App Expression Expression
    deriving Eq

instance Show Expression where
    show exp = case exp of
        Abs v (App m n)      -> 'λ':v ++ '.':show m ++ ' ':show n
        Abs v x              -> 'λ':v ++ '.':show x
        Var v                -> v
        App m n              -> '(':show m ++ ' ':show n ++ ")"

-- Returns a list of the subexpressions of the given tree node
subs :: Expression -> [Expression]
subs (Abs _ x)          = [x]
subs (App x y)          = [x, y]
subs _                  = []

-- Creates a list of all name occurences in an Expression - If a name is used
-- twice, it will appear twice in the returned list, etc.
names :: Expression -> [Name]
names (Abs n x) = n : names x
names (Var n)   = [n]
names (App m n) = names m ++ names n

-- Returns true if there are any occurences (free or bound) of the given string
-- as a name in the given Expression. Used by newName to generate names that are
-- not found in a given expression.
nameIn :: Name -> Expression -> Bool
nameIn n (Abs n' x) = (n == n') || nameIn n x
nameIn n (Var n')   = n == n'
nameIn n exp        = any (nameIn n) (subs exp)

-- Creates a set of all free variables in an Expression, which can be used when
-- reducing a function application, as it determines which names will need to be
-- changed in order to prevent name clashes.
freeVars :: Expression -> Set.Set Name
freeVars exp = case exp of
    Var x   -> Set.singleton x
    Abs x m -> Set.delete x (freeVars m)
    _       -> Set.unions $ map freeVars $ subs exp

-- Composes Set.toList with freeVars, yeilding a list of the free names in an
-- Expression, as opposed to a Set
freeNames :: Expression -> [Name]
freeNames = Set.toList . freeVars
