-- This module defines the abstract syntax of the language, and a few basic
-- functions over them, which retrieve information from them.
module TypedSyntax where

import qualified Data.Set as Set

type Name = String

data TypedExp = Abs Name Type TypedExp
              | Var Name
              | App TypedExp TypedExp
              | Constant Type Value
              | Arithmetic TypedExp Op TypedExp
    deriving Eq

instance Show TypedExp where
    show exp = case exp of
        Abs v t (Arithmetic x op y)
            -> concat ['λ':v, " : ", show t, " . ", show x, ' ':show op, ' ':show y]
        Abs v t (App m n)   -> concat ['λ':v, " : ", show t, '.':show m, ' ':show n]
        Abs v t x           -> concat ['λ':v, " : ", show t, '.':show x]

        Var v               -> v
        App m n             -> '(':show m ++ ' ':show n ++ ")"
        Constant t v        -> '(':show v ++ " : " ++ show t ++ ")"
        Arithmetic m op n   -> '(':show m ++ ' ':show op ++ ' ':show n ++ ")"

-- TypeValue represents a constant value that cannot have a function type. The
-- possible constant values can be integers, floats, chars and booleans.
data Value = IntVal   Int
           | CharVal  Char
           | FloatVal Float
           | BoolVal  Bool
    deriving Eq

instance Show Value where
    show val = case val of
        IntVal   x -> show x
        CharVal  x -> show x
        FloatVal x -> show x
        BoolVal  x -> show x

-- Type is a recursive data type used for representing the types of functions
data Type = TInt
          | TChar
          | TFloat
          | TBool
          | TList Type
          | TFunc Type Type
    deriving Eq

instance Show Type where
    show t = case t of
        TInt      -> "Int"
        TChar     -> "Char"
        TFloat    -> "Float"
        TBool     -> "Bool"
        TList a   -> '[':show a ++ "]"
        TFunc a b -> show a ++ " -> " ++ show b

-- The Op data type represents the possible kinds of arithmetic operation that
-- can be performed.
data Op = Add | Sub | Mul | Div | Mod
    deriving Eq

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Mod = "%"

-- Retrieve the function that corresponds to the given Op
getOp :: (Integral a, Num a) => Op -> (a -> a -> a)
getOp Add = (+)
getOp Sub = (-)
getOp Mul = (*)
getOp Div = div
getOp Mod = mod

-- Returns a list of the subexpressions of the given tree node
subs :: TypedExp -> [TypedExp]
subs (Abs _ _ x)        = [x]
subs (App x y)          = [x, y]
subs (Arithmetic x _ y) = [x, y]
subs _                  = []

-- Creates a list of all name occurences in an TypedExp - If a name is used
-- twice, it will appear twice in the returned list, etc.
names :: TypedExp -> [Name]
names (Abs n _ x)    = n : names x
names (Var n)        = [n]
names (Constant _ _) = []
names exp            = concat (map names (subs exp))

-- Returns true if there are any occurences (free or bound) of the given string
-- as a name in the given TypedExp. Used by newName to generate names that are
-- not found in a given expression.
nameIn :: Name -> TypedExp -> Bool
nameIn n (Abs n' _ x)   = (n == n') || nameIn n x
nameIn n (Var n')       = n == n'
nameIn _ (Constant _ _) = False
nameIn n exp            = any (nameIn n) (subs exp)

-- Creates a set of all free variables in a TypedExp, which can be used when
-- reducing a function application, as it determines which names will need to be
-- changed in order to prevent name clashes.
freeVars :: TypedExp -> Set.Set Name
freeVars exp = case exp of
    Var x        -> Set.singleton x
    Abs x _ m    -> Set.delete x (freeVars m)
    Constant _ _ -> Set.empty
    _            -> Set.unions $ map freeVars $ subs exp

-- Composes Set.toList with freeVars, yeilding a list of the free names in a
-- TypedExp, as opposed to a Set
freeNames :: TypedExp -> [Name]
freeNames = Set.toList . freeVars
