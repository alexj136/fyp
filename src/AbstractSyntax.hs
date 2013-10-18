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

-- Retrieve the function that corresponds to the given Op
doOp :: (Integral a, Num a) => Op -> (a -> a -> a)
doOp Add = (+)
doOp Sub = (-)
doOp Mul = (*)
doOp Div = div
doOp Mod = mod

-- Returns a list of the subexpressions of the given tree node
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
(===) (Literal x)         (Literal y)          = x == y
(===) (Name n)            (Name n')            = n == n'
(===) (Lambda n x)        (Lambda n' y)        = x === renamedY
    where renamedY = head (subs (renameBound n' n (Lambda n' y)))
(===) (Application x y)   (Application a b)    = x === a && y === b
(===) (Arithmetic x op y) (Arithmetic a op' b) = x === a && op == op' && y === b
(===) (IfThenElse x y z)  (IfThenElse a b c)   = x === a && y === b   && z === c

-- Creates a list of all name occurences in an Expression - If a name is used
-- twice, it will appear twice in the returned list, etc.
names :: Expression -> [String]
names (Lambda n x) = n : names x
names (Name n)     = [n]
names (Literal _)  = []
names exp          = concat (map names (subs exp))

-- Creates a Set of all names that occur in an Expression
nameSet :: Expression -> Set.Set String
nameSet = Set.fromList . names

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
freeNamesSet :: Set.Set String -> Set.Set String -> Expression -> Set.Set String
freeNamesSet free _     (Literal _)  = free
freeNamesSet free bound (Name n)     | Set.member n bound = free
                                     | otherwise          = Set.insert n free
freeNamesSet free bound (Lambda n x) = freeNamesSet free (Set.insert n bound) x
freeNamesSet free bound exp          =
    Set.unions (map (freeNamesSet free bound) (subs exp))

-- Calls freeNamesSet with empty sets and a given expression, and converts the
-- resulting set to a list. Makes calls from other files simpler, removing the
-- need for them to import Data.Set.
freeNames :: Expression -> [String]
freeNames exp = Set.toList (freeNamesSet Set.empty Set.empty exp)

-- Returns a string which does not exist as a name within a given Expression.
-- Begins by choosing the name "x", and if that already exists in the given
-- expression, add a prime (') character to it. Keep adding prime characters
-- until we have a name for which the nameIn function will return false.
newName :: Expression -> String
newName exp | nameIn "x" exp = newName2 "x" exp
            | otherwise      = "x"
    where newName2 lastTry exp
              | nameIn lastTry' exp = newName2 lastTry' exp
              | otherwise = lastTry'
                  where lastTry' = lastTry ++ "'"

-- Generates a String that does not exist in the given set of strings. Works in
-- the same way as newName.
newNameSet :: Set.Set String -> String
newNameSet set | Set.member "x" set = newNameSet2 "x" set
               | otherwise          = "x"
    where newNameSet2 lastTry set
              | Set.member lastTry' set = newNameSet2 lastTry' set
              | otherwise               = lastTry'
                  where lastTry' = lastTry ++ "'"

-- Generates a list of Strings, none of which appear in the given set of Strings
newNameSets :: Set.Set String -> [String]
newNameSets set = newNameSets2 set [] (Set.size set)
    where newNameSets2 set lst count
              | count == 0 = lst
              | otherwise  = newNameSets2 (Set.insert n set) (n : lst) (count - 1)
                  where n = newNameSet set

-- Returns true if there are any occurences (free or bound) of the given string
-- as a name in the given Expression. Used by newName to generate names that are
-- not found in a given expression.
nameIn :: String -> Expression -> Bool
nameIn n (Lambda n' x) = (n == n') || nameIn n x
nameIn n (Name n')     = n == n'
nameIn _ (Literal _)   = False
nameIn n exp           = any (nameIn n) (subs exp)

-- Rename every occurence in the given Expression, of every name in the given
-- list, such that no name clashes can occur if Expressions containing names in
-- the given list are substituted into the given Expression.
renameAll :: [String] -> Expression -> Expression
renameAll []    x = x
renameAll (h:t) x = renameAll t (renameBound h (newName x) x)

-- Rename only the bound instances of the given name in the given expression.
-- Recurse down the tree until we find the given binding, at which point we can
-- do a 'blind' rename of every instance of that name that we find, as they are
-- guaranteed to be bound, either by the binding we just found, or by a another
-- binding of the same name. In both cases, we want to rename, because either
-- could potentially clash with a substituted expression. If we find the name
-- before we find the binding, we don't need to do anything - the name is not
-- bound at that point and therefore doesn't need renaming.
renameBound :: String -> String -> Expression -> Expression
renameBound _    _  (Literal x) = Literal x
renameBound _    _  (Name n)    = Name n
renameBound from to exp         = case exp of
    Lambda n x | n == from -> Lambda to (blindRename from to x)
               | otherwise -> Lambda n (renamed x)
    Application x x'       -> Application (renamed x) (renamed x')
    Arithmetic x op x'     -> Arithmetic (renamed x) op (renamed x')
    IfThenElse x x' x''    -> IfThenElse (renamed x) (renamed x') (renamed x'')
    where renamed = renameBound from to

-- Renames every name with value 'from' to value 'to' in an Expression, with no
-- regard for clashes. It is assumed that this function will only be called on
-- Expressions where the potential for incorrectly renaming unbound names has
-- been eliminated.
blindRename :: String -> String -> Expression -> Expression
blindRename from to exp = case exp of
    Lambda n x | n == from -> Lambda to (renamed x)
               | otherwise -> Lambda n (renamed x)
    Name n     | n == from -> Name to
               | otherwise -> Name n
    Application x x'       -> Application (renamed x) (renamed x')
    Arithmetic x op x'     -> Arithmetic (renamed x) op (renamed x')
    IfThenElse x x' x''    -> IfThenElse (renamed x) (renamed x') (renamed x'')
    Literal x              -> Literal x
    where renamed = blindRename from to
