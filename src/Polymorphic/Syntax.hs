-- This module defines the abstract syntax of the language, and a few basic
-- functions over them, which retrieve information from them.
module Syntax where

import qualified Data.Set as S

--------------------------------------------------------------------------------
--                         ESSENTIAL DATA DEFINITIONS
--------------------------------------------------------------------------------

type Name = String

data TypedExp
    = Abs Name Type TypedExp -- Abstraction labelled with a type
    | AbsInf Name TypedExp   -- Unlabelled Abs - type is inferred
    | Var Name               -- Variable
    | App TypedExp TypedExp  -- Function application
    | Constant Value         -- Integer & boolean constants
    | Operation OpType       -- Operators like +, - etc
    deriving Eq

instance Show TypedExp where
    show exp = case exp of
        -- First case just stops brackets from appearing around applications
        -- that appear immidiately within an abstraction
        Abs v t (App m n)  ->
            concat ['λ' : v, " : ", show t, '.' : show m, ' ' : show n]
        Abs v t x          -> concat ['λ' : v, " : ", show t, '.' : show x]
        AbsInf v (App m n) -> concat ['λ' : v, " . ", show m, ' ' : show n]
        AbsInf v x         -> concat ['λ' : v, " . ", show x]
        Var v              -> v
        App m n            -> '(':show m ++ ' ':show n ++ ")"
        Constant v         -> show v
        Operation ot       -> show ot

-- Value represents a constant value. The possible constant values can be
-- integers, floats, chars and booleans.
data Value = BoolVal Bool
           | IntVal  Int
           | CharVal Char
    deriving Eq

instance Show Value where
    show val = case val of
        BoolVal x -> show x
        IntVal  x -> show x
        CharVal x -> show x

-- Type is a recursive data type used for representing the types of functions
data Type
    = TInt
    | TBool
    | TChar
    | TList  Type
    | TFunc  Type Type
    | TVar   Int       -- Type variables are numbers, not strings
    | TQuant Int Type  -- Type quantifier
    deriving Eq

instance Show Type where
    show t = case t of
        TInt       -> "Int"
        TBool      -> "Bool"
        TChar      -> "Char"
        TList a    -> '[' : show a ++ "]"
        TFunc a b  -> show a ++ " -> " ++ show b
        TVar varNo -> 'T' : show varNo
        TQuant i t -> 'V' : (show i) ++ '.' : (show t)

-- It is helpful to make Types orderable so that manipulating large sets of
-- Types is faster
instance Ord Type where
    (<=) a b = case ( a , b ) of
        -- Every type is greater than Bool
        ( TBool       , _           ) -> True

        -- Only Bool is less than Int
        ( TInt        , TBool       ) -> False
        ( TInt        , _           ) -> True

        -- Bool and Int are less than Char
        ( TChar       , TBool       ) -> False
        ( TChar       , TInt        ) -> False
        ( TChar       , _           ) -> True

        -- Lists are greater than Bool, Int & Char, but less than everything
        -- else. List A is less than or equal to List B if A is less than or
        -- equal to List B.
        ( TList _     , TBool       ) -> False
        ( TList _     , TInt        ) -> False
        ( TList _     , TChar       ) -> False
        ( TList t1    , TList t2    ) -> t1 <= t2
        ( TList _     , _           ) -> True

        -- TFunc is only less than TVar and TQuant. The argument type determines
        -- which of two TFuncs are greater. If the argument types are equal,
        -- then the return type determines the greater one.
        ( TFunc t1 t2 , TFunc t3 t4 ) -> if t1 == t3 then t2 <= t4 else t1 <= t3
        ( TFunc _ _   , TVar _      ) -> True
        ( TFunc _ _   , TQuant _ _  ) -> True
        ( TFunc _ _   , _           ) -> False

        -- TVars are greater than everything but TQuants
        ( TVar v1     , TVar v2     ) -> v1 <= v2
        ( TVar _      , TQuant _ _  ) -> True
        ( TVar _      , _           ) -> False

        -- Type quantifiers are greater than everything. The bound variable
        -- takes precedence over the body.
        ( TQuant x t1 , TQuant y t2 ) -> if x == y then t1 <= t2 else x <= y

-- The Op data type represents the possible kinds of arithmetic operation that
-- can be performed.
data OpType
    -- Operations on Int & Bool primitives
    = Add | Sub | Mul | Div | Mod       -- Int  -> Int  -> Int
    | Lss | LsE | Equ | NEq | Gtr | GtE -- Int  -> Int  -> Bool
    | And | Or  | Xor                   -- Bool -> Bool -> Bool
    | Not                               -- Bool -> Bool
    | IsZ                               -- Int  -> Bool
    
    -- List operations
    | Empty     -- The empty list       : [a]
    | Cons      -- List constructor     :  a  -> [a] -> [a]
    | Null      -- Is-empty function    : [a] -> Bool
    | Head      -- Head function        : [a] ->  a
    | Tail      -- Tail function        : [a] -> [a]
    deriving Eq

-- Tell if an operation type is binary. Will not need to change in
-- implementation as long as operations with more than two arguments are
-- implemented.
isBinary :: OpType -> Bool
isBinary op = case op of
    Add -> True
    Sub -> True
    Mul -> True
    Div -> True
    Mod -> True
    
    And -> True
    Or  -> True
    Xor -> True
    
    Lss -> True
    LsE -> True
    Equ -> True
    NEq -> True
    Gtr -> True
    GtE -> True
    
    _   -> False

-- Tell if an operation is unary. Just the negation of isBinary.
isUnary :: OpType -> Bool
isUnary op = case op of
    Not -> True
    IsZ -> True
    _   -> False

instance Show OpType where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Mod = "%"

    show And = "&"
    show Or  = "|"
    show Xor = "#"

    show Lss = "<"
    show LsE = "<="
    show Equ = "=="
    show NEq = "/="
    show Gtr = ">"
    show GtE = ">="

    show Not = "!"
    show IsZ = "isZero"

    show Empty = "[]"
    show Cons  = ":"
    show Null  = "null"
    show Head  = "head"
    show Tail  = "tail"

--------------------------------------------------------------------------------
--                  FUNCTIONS TO GAIN INFORMATION ON TERMS
--------------------------------------------------------------------------------

-- Returns a list of the subexpressions of the given tree node
subs :: TypedExp -> [TypedExp]
subs (Abs _ _ x)  = [x]
subs (AbsInf _ x) = [x]
subs (App x y)    = [x, y]
subs _            = []

-- Creates a list of all name occurences in an TypedExp - If a name is used
-- twice, it will appear twice in the returned list, etc.
names :: TypedExp -> [Name]
names (Abs n _ x)  = n : names x
names (AbsInf n x) = n : names x
names (Var n)      = [n]
names (Constant _) = []
names exp          = concat (map names (subs exp))

-- Returns true if there are any occurrences (free or bound) of the given string
-- as a name in the given TypedExp. Used by newName to generate names that are
-- not found in a given expression.
nameIn :: Name -> TypedExp -> Bool
nameIn n (Abs n' _ x)  = (n == n') || nameIn n x
nameIn n (AbsInf n' x) = (n == n') || nameIn n x
nameIn n (Var n')      = n == n'
nameIn _ (Constant _)  = False
nameIn n exp           = any (nameIn n) (subs exp)

-- Creates a set of all free variables in a TypedExp, which can be used when
-- reducing a function application, as it determines which names will need to be
-- changed in order to prevent name clashes.
freeVars :: TypedExp -> S.Set Name
freeVars exp = case exp of
    Var x      -> S.singleton x
    Abs x _ m  -> S.delete x (freeVars m)
    AbsInf x m -> S.delete x (freeVars m)
    Constant _ -> S.empty
    _          -> S.unions $ map freeVars $ subs exp

-- Composes S.toList with freeVars, yielding a list of the free names in a
-- TypedExp, as opposed to a Set
freeNames :: TypedExp -> [Name]
freeNames = S.toList . freeVars

maxTVarInExp :: TypedExp -> Int 
maxTVarInExp exp = maximum (0 : map maxTVar typesInExp)
    where typesInExp = getTypesInExp exp

-- Get a list containing all types that occur within an expression
getTypesInExp :: TypedExp -> [Type]
getTypesInExp (Abs _ t m) = t : getTypesInExp m
getTypesInExp exp         = concat (map getTypesInExp (subs exp))

--------------------------------------------------------------------------------
--                  FUNCTIONS TO GAIN INFORMATION ON TYPES
--------------------------------------------------------------------------------

-- Get a list of all type variable names in a type. The order of the list
-- the order in which those type variables appear in the type.
tVarNames :: Type -> [Int]
tVarNames (TQuant x t)  = x : tVarNames t
tVarNames (TVar x)      = [x]
tVarNames (TFunc t1 t2) = tVarNames t1 ++ tVarNames t2
tVarNames (TList t)     = tVarNames t
tVarNames _             = []

-- Converts all occurences of a particular TVar name to a different name within
-- a type
renameType :: Int -> Int -> Type -> Type
renameType from to (TQuant x t)  =
    TQuant (if x == from then to else x) (renameType from to t)
renameType from to (TVar x)      = TVar (if x == from then to else x)
renameType from to (TFunc t1 t2) = TFunc renamedT1 renamedT2
    where renamedT1 = renameType from to t1
          renamedT2 = renameType from to t2
renameType from to (TList t)     = TList (renameType from to t)
renameType _    _  other         = other

-- Rename all occurences of all the types in the given list to another type.
-- The integer parameter is the first TVar name that will be used to replace old
-- ones, and the next used will be its successor, etcetera.
renameAllTypes :: Int -> [Int] -> Type -> Type
renameAllTypes _     []      ty = ty
renameAllTypes newTy (hd:tl) ty =
    renameAllTypes (newTy + 1) tl (renameType hd newTy ty)

-- Determine the highest TVar number used in a type - defaults to zero if none
-- are used
maxTVar :: Type -> Int
maxTVar = (foldl max 0) . tVarNames

-- Check if two types are equivalent - similar to α-equivalence, but for type
-- variable names.
typeEquiv :: Type -> Type -> Bool
typeEquiv t1 t2 =
    renameAllTypes next namesT1 t1 == renameAllTypes next namesT2 t2
    where
        next = 1 + max (maxTVar t1) (maxTVar t2)
        namesT1 = tVarNames t1
        namesT2 = tVarNames t2
