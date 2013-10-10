-- This module defines the abstract syntax of the language
module AbstractSyntax where

import qualified Data.Set as Set

-- Lambda Expressions need a mechanism to handle naming clashes when replacing
-- variables (bound variables with mathcing names should be renamed
data Expression = Lambda String Expression
                | Name String
                | Application Expression Expression
-- Churchy Expressions:
                | Literal Int
                | Arithmetic Expression Op Expression
                | IfThenElse Expression Expression Expression
    deriving (Show, Eq)

data Op = Add | Sub | Mul | Div | Mod
    deriving (Show, Eq)

-- Returns a list of all subexpressions contained in an Expression
subs :: Expression -> [Expression]
subs (Lambda _ x)       = [x]
subs (Application x y)  = [x, y]
subs (Arithmetic x _ y) = [x, y]
subs (IfThenElse x y z) = [x, y, z]
subs _                  = []

-- The === operator defines alpha-equivalence of expressions, i.e. are two
-- Expressions equal if we disregard names. Some examples:
-- \x.x   === \y.y   -> True
-- \xy.yx === \sz.zs -> True
-- \ab.ac === \xy.yz -> False
(===) :: Expression -> Expression -> Bool

(===) (Arithmetic x op y) (Arithmetic x' op' y') = all [x === x', y === y', op == op']
(===) (Literal x)         (Literal y)            = x == y
(===) _                   _                      = False

-- Enumerates all names used in an Expression regardless of how they are used
names :: Expression -> [String]
names (Lambda n x)          = n : names x
names (Name n)              = [n]
names (Application x x')    = (names x) ++ (names x')
names (Arithmetic x op x')  = (names x) ++ (names x')
names (IfThenElse x x' x'') = (names x) ++ (names x') ++ (names x'')
names (Literal _)           = []

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
freeNames :: Set.Set String -> Set.Set String -> Expression -> Set.Set String
freeNames free bound (Name n)              | Set.member n bound = free
                                           | otherwise          = Set.insert n free
freeNames free bound (Lambda n x)          = freeNames free (Set.insert n bound) x
freeNames free bound (Application x x')    = Set.unions (map (freeNames free bound) [x, x'])
freeNames free bound (Arithmetic x _ x')   = Set.unions (map (freeNames free bound) [x, x'])
freeNames free bound (IfThenElse x x' x'') = Set.unions (map (freeNames free bound) [x, x', x''])
freeNames free bound _                     = free

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
