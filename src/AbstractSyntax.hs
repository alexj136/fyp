-- This module defines the abstract syntax of the language, and a few basic
-- functions over them, which retrieve information from them.
module AbstractSyntax where

import qualified Data.Set as Set

type Name = String

-- Lambda Expressions need a mechanism to handle naming clashes when replacing
-- variables (bound variables with mathcing names should be renamed
data Expression = Abs Name Expression
                | TAbs Name Type Expression
                | Var Name
                | App Expression Expression
                | Constant TypeValue
                | Arithmetic Expression Op Expression
    deriving Eq

instance Show Expression where
    show exp = case exp of
        Abs v (Arithmetic x op y)
            -> 'λ':v ++ '.':show x ++ ' ':show op ++ ' ':show y
        Abs v (App m n)      -> 'λ':v ++ '.':show m ++ ' ':show n
        Abs v x              -> 'λ':v ++ '.':show x

        TAbs v t (Arithmetic x op y)
            -> concat ['λ':v, " : ", show t, " . ", show x, ' ':show op, ' ':show y]
        TAbs v t (App m n)   -> 'λ':v ++ '.':show m ++ ' ':show n
        TAbs v t x           -> 'λ':v ++ '.':show x

        Var v                -> v
        App m n              -> '(':show m ++ ' ':show n ++ ")"
        Constant c           -> show c
        Arithmetic m op n    -> '(':show m ++ ' ':show op ++ ' ':show n ++ ")"

data TypeValue = CInt   Int
               | CChar  Char
               | CFloat Float
               | CBool  Bool
    deriving Eq

instance Show TypeValue where
    show typeVal = case typeVal of
        CInt   x -> show x ++ " : Int"
        CChar  x -> show x ++ " : Char"
        CFloat x -> show x ++ " : Float"
        CBool  x -> show x ++ " : Bool"

data Type = TInt | TChar | TFloat | TBool | TFunc Type Type
    deriving Eq

instance Show Type where
    show t = case t of
        TInt      -> "Int"
        TChar     -> "Char"
        TFloat    -> "Float"
        TBool     -> "Bool"
        TFunc a b -> show a ++ " -> " ++ show b

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
subs :: Expression -> [Expression]
subs (Abs _ x)          = [x]
subs (App x y)          = [x, y]
subs (Arithmetic x _ y) = [x, y]
subs _                  = []

-- Creates a list of all name occurences in an Expression - If a name is used
-- twice, it will appear twice in the returned list, etc.
names :: Expression -> [Name]
names (Abs n x)    = n : names x
names (Var n)      = [n]
names (Constant _) = []
names exp          = concat (map names (subs exp))

-- Returns true if there are any occurences (free or bound) of the given string
-- as a name in the given Expression. Used by newName to generate names that are
-- not found in a given expression.
nameIn :: Name -> Expression -> Bool
nameIn n (Abs n' x)   = (n == n') || nameIn n x
nameIn n (Var n')     = n == n'
nameIn _ (Constant _) = False
nameIn n exp          = any (nameIn n) (subs exp)

-- Creates a set of all free variables in an Expression, which can be used when
-- reducing a function application, as it determines which names will need to be
-- changed in order to prevent name clashes.
freeVars :: Expression -> Set.Set Name
freeVars exp = case exp of
    Var x      -> Set.singleton x
    Abs x m    -> Set.delete x (freeVars m)
    Constant _ -> Set.empty
    _          -> Set.unions $ map freeVars $ subs exp

-- Composes Set.toList with freeVars, yeilding a list of the free names in an
-- Expression, as opposed to a Set
freeNames :: Expression -> [Name]
freeNames = Set.toList . freeVars
