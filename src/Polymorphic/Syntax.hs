-- This module defines the abstract syntax of the language, and a few basic
-- functions over them, which retrieve information from them.
module Syntax where

import qualified Data.Set as S
import qualified Data.Map as M

--------------------------------------------------------------------------------
--                            PROGRAMS & FUNCTIONS
--------------------------------------------------------------------------------

-- Programs are represented with the 'Prog' datatype, which is a wrapper around
-- a map (Data.Map) from function names to functions. This allows the
-- interpreter to easily retrieve them as required.

type Name = String

data Prog = Prog (M.Map Name Func)
    deriving (Eq, Ord)

instance Show Prog where
    show (Prog p)
        | M.null p  = ""
        | otherwise =
            let ((_, f), rest) = M.deleteFindMin p in
                show f ++ "\n\n" ++ show (Prog rest)

-- Create a new program containing a single function
newProg :: Func -> Prog
newProg f = Prog (M.singleton (getName f) f)

-- The empty program
nilProg :: Prog
nilProg = Prog M.empty

-- Convert a list of functions into a Prog object
progFromList :: [Func] -> Prog
progFromList funcs = progFromListAcc funcs M.empty
  where
    progFromListAcc []     pgMap = Prog pgMap
    progFromListAcc (f:fs) pgMap = case M.lookup (getName f) pgMap of
        Nothing -> progFromListAcc fs (M.insert (getName f) f pgMap)
        Just _  -> error ("progFromList: Multiple definitions of " ++
                        (getName f))

-- Get the size of a Prog
progSize (Prog pg) = M.size pg

-- Determine if a Prog contains a Func of a particular name
hasFunc :: Prog -> Name -> Bool
hasFunc (Prog p) nm = M.member nm p

getFunc :: Prog -> Name -> Func
getFunc (Prog p) nm = case M.lookup nm p of
    Just f  -> f
    Nothing -> error ("getFunc: No definition for '" ++ nm ++ "' found")

-- Add a new function to a program - this will be repeatedly called by the
-- parser when building a program. If more than one function is added with a
-- specific name, an error is raised and the user is notified.
addFunc :: Func -> Prog -> Prog
addFunc f (Prog p)
    | hasFunc (Prog p) nm = error ("addFunc: Multiple definitions of " ++ nm)
    | otherwise           = Prog (M.insert nm f p)
    where nm = getName f

data Func
    = Func Type Name [Name] Term
    | FuncInf   Name [Name] Term
    deriving (Eq, Ord)

-- Get the name of a function
getName :: Func -> Name
getName (Func  _ n _ _) = n
getName (FuncInf n _ _) = n

-- Get the body of a function
getBody :: Func -> Term
getBody (Func  _ _ _ b) = b
getBody (FuncInf _ _ b) = b

-- Get the number of arguments of a function
numArgs :: Func -> Int
numArgs (Func  _ _ a _) = length a
numArgs (FuncInf _ a _) = length a

instance Show Func where
    show (Func ty nm args body) = show ty ++ " :\n" ++ nm ++ showArgs args ++
                                  " = " ++ show body
    show (FuncInf nm args body) = nm ++ showArgs args ++ " = " ++ show body

-- Stringify a list of argument names
showArgs :: [Name] -> String
showArgs []     = "" 
showArgs (x:xs) = ' ' : x ++ showArgs xs

-- Make a new function from its components, taking a Maybe Type to determine if
-- a Func or a FuncInf should be used
makeFunc :: Name -> [Name] -> Term -> Func
makeFunc nm args body = FuncInf nm args body

-- Determine if a Func has a type label or not (i.e. if its constructor is Func
-- as opposed to FuncInf)
hasTypeLabel :: Func -> Bool
hasTypeLabel (Func  _ _ _ _) = True
hasTypeLabel (FuncInf _ _ _) = False

-- Add a type label to an unlabelled function. If the function already has a
-- label, raise an error.
addTypeLabel :: Func -> Type -> Func
addTypeLabel (FuncInf nm args bdy) ty = Func ty nm args bdy
addTypeLabel (Func _  nm _    _  ) _  = error ("addTypeLabel: Multiple type" ++
    " declarations for function: " ++ nm)

-- Get the type label from a labelled function. If the given function is
-- unlabelled, then raise an error.
getTypeLabel :: Func -> Type
getTypeLabel (Func ty _ _ _) = ty
getTypeLabel _               = error ("getTypeLabel: tried to get type " ++
                                      "label of unlabelled function")

-- Convert functions with many arguments to pure lambda expressions by
-- replacing arguments with abstractions inside the function body. This is
-- required for interpretation, but is not desirable when compiling.
toLambdas :: Func -> Term
toLambdas f = case f of
    Func ty nm []   body -> App (Operation Fix) (Abs nm ty body)
    FuncInf nm []   body -> App (Operation Fix) (AbsInf nm body)
    Func ty nm args body ->
        toLambdas (Func ty nm (init args) (AbsInf (last args) body))
    FuncInf nm args body ->
        toLambdas (FuncInf nm (init args) (AbsInf (last args) body))

-- Convert an entire program into a single lambda term that can be given to the
-- unification algorithm. This will convert something that looks like:
--      f x = 2 * x
--      g y = y + f y
--      main = g (g y)
--  into something that looks like:
--      let f x = 2 * x   in
--      let g y = y + f x in
--          g (g y)
--  which is syntactic sugar for:
--      (^f . (^g . (g (g y)) ^y . (f y))) (^x . (2 * x))
allToLambdas :: Prog -> Term
allToLambdas pg
    -- If a program has no functions, we can't convert it to a lambda term
    | nilProg == pg = error "allToLambdas: Empty program"

    -- If a program has no main function, we don't know which function should be
    -- the 'body' of our converted term, so fail
    | not (hasFunc pg "main") = error "allToLambdas: No main function found"

    -- If a program has a main function, but that function takes arguments, then
    -- fail, because our convention is that the main function takes no
    -- arguments.
    | numArgs (getFunc pg "main") /= 0 =
        error "allToLambdas: main function must take exactly 0 arguments"

    -- If a program has a main function, and that is the only function, and that
    -- function takes zero arguments (as per our convention for the main
    -- function), just return the body of that function
    | progSize pg == 1 && numArgs (getFunc pg "main") == 0 =
        getBody (getFunc pg "main")

    -- If we passed all above tests, we have a valid main function, plus some
    -- other function(s). Take one of the non-main functions, and incorporate it
    -- into the body of the main function, removing it from the map of
    -- functions. Then recurse.
    | progSize pg > 1 =
        let Prog pgMap = pg
            oldMain = getFunc pg "main"
            nxtFunc =
                if (fst (M.elemAt 0 pgMap) == "main") then
                    snd (M.elemAt 1 pgMap)
                else
                    snd (M.elemAt 0 pgMap)
            absOfNxtFunc = 
                if hasTypeLabel nxtFunc then
                    (Abs (getName nxtFunc) (getTypeLabel nxtFunc)
                        (getBody oldMain)
                    )
                else
                    (AbsInf (getName nxtFunc) (getBody oldMain))
            newMain =
                if hasTypeLabel oldMain then
                    Func (getTypeLabel oldMain) "main" []
                        (App absOfNxtFunc (toLambdas nxtFunc))
                else
                    FuncInf "main" []
                        (App absOfNxtFunc (toLambdas nxtFunc))
        in
            allToLambdas (Prog (
                M.insert "main" newMain (M.delete (getName nxtFunc) pgMap)
            ))

--------------------------------------------------------------------------------
--                    EXPRESSIONS - THE 'Term' DATATYPE
--------------------------------------------------------------------------------

-- Terms are the expressions of the language - they comprise function bodies.
data Term
    = Abs    Name Type Term -- Abstraction labelled with a type
    | AbsInf Name      Term -- Unlabelled Abs - type is inferred
    | Var    Name           -- Variable
    | App    Term Term      -- Function application
    | Constant  Value       -- Integer & boolean constantserm
    | Operation OpType      -- Operators like +, - etc
    deriving (Eq, Ord)

instance Show Term where
    show exp = case exp of
        -- Show strings in "the traditional way". Will fail if the list contains
        -- anything other than characters, however this will never be the case
        -- for an expression that passes type-checking.
        --App (App (Operation Cons) (Constant (CharVal c))) chars ->
            --showAsString exp

        -- Show lists in [the, traditional, way]
        --App (App (Operation Cons) elem) rest -> showAsList exp

        -- Prevent bracketing around lists
        --App m (App (App n o) p) | n == Operation Cons ->
            --show m ++ ' ' : show (App (App n o) p)

        -- Extra rule to capture the left-associativity of function application
        -- throught bracketing
        App m (App n o) -> show m ++ " (" ++ show (App n o) ++ ")"

        App m n         -> show m ++ ' ' : show n
        Abs v t x       -> concat ['\\' : v, " : ", show t, '.' : show x]
        AbsInf v x      -> concat ['\\' : v, " . ", show x]
        Var v           -> v
        Constant v      -> show v
        Operation ot    -> show ot

-- Get a nice representation of an encoded character string - will fail for
-- expressions that are not proper strings (i.e. that do not yeild 'TList TChar'
-- in a type check.
{--showAsString :: Term -> String
showAsString str = show (toHaskellString str)
  where
    toHaskellString :: Term -> String
    toHaskellString (App (App (Operation Cons) (Constant (CharVal c))) rest) =
        c : toHaskellString rest
    toHaskellString (Operation Empty) = ""
    toHaskellString _                 =
        error "Tried to show an ill-typed string"

-- Get a nice representation of an encoded list
showAsList :: Term -> String
showAsList str = show (toHaskellList str)
  where
    toHaskellList :: Term -> [Term]
    toHaskellList (App (App (Operation Cons) exp) rest) =
        exp : toHaskellList rest
    toHaskellList (Operation Empty) = []
    toHaskellList _                 =
        error "Tried to show an ill-typed list"--}

-- Value represents a constant value. The possible constant values can be
-- integers, floats, chars and booleans.
data Value = BoolVal Bool
           | IntVal  Int
           | CharVal Char
    deriving (Eq, Ord)

instance Show Value where
    show val = case val of
        BoolVal x -> show x
        IntVal  x -> show x
        CharVal x -> show x

-- The Op data type represents the possible kinds of arithmetic operation that
-- can be performed.
data OpType
    -- Operations on Int & Bool primitives
    = Add | Sub | Mul | Div | Mod       -- : Int  -> Int  -> Int
    | Lss | LsE | Equ | NEq | Gtr | GtE -- : Int  -> Int  -> Bool
    | And | Or  | Xor                   -- : Bool -> Bool -> Bool
    | Not                               -- : Bool -> Bool
    | IsZ                               -- : Int  -> Bool
    
    -- List operations
    | Empty     -- The empty list       : Va.[a]
    | Cons      -- List constructor     : Va. a  -> [a] -> [a]
    | Null      -- Is-empty function    : Va.[a] -> Bool
    | Head      -- Head function        : Va.[a] ->  a
    | Tail      -- Tail function        : Va.[a] -> [a]

    -- Conditionals i.e. if-then-else
    | Cond      -- : Va.Bool -> a -> a -> a

    -- Fixed-point combinator
    | Fix       -- : Va.(a -> a) -> a

    -- Sum injection & removal operators
    | InjL      -- : Va, b. a -> { a | b }
    | InjR      -- : Va, b. b -> { a | b }
    | RemL      -- : Va, b. { a | b } -> a
    | RemR      -- : Va, b. { a | b } -> b

    -- Product injection & removal
    | Tuple     -- Va, b. a -> b -> { a & b }
    | Fst       -- Va, b. { a & b } -> a
    | Snd       -- Va, b. { a & b } -> b
    deriving (Eq, Ord)

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

-- Tell if an operation is unary
isUnary :: OpType -> Bool
isUnary op = case op of
    Not -> True
    IsZ -> True
    _   -> False

instance Show OpType where
    show ot = case ot of
        { Add   -> "+"    ; Sub  -> "-"    ; Mul  -> "*"    ; Div   -> "/"
        ; Mod   -> "%"    ; Lss  -> "<"    ; LsE  -> "<="   ; Equ   -> "=="
        ; NEq   -> "/="   ; Gtr  -> ">"    ; GtE  -> ">="   ; And   -> "and"
        ; Or    -> "or"   ; Xor  -> "xor"  ; Not  -> "not"  ; IsZ   -> "iszero"
        ; Empty -> "[]"   ; Cons -> ":"    ; Null -> "null" ; Head  -> "head"
        ; Tail  -> "tail" ; Fix  -> "Y"    ; Cond -> "cond" ; InjL  -> "injl"
        ; InjR  -> "injr" ; RemL -> "reml" ; RemR -> "remr" ; Tuple -> "tuple"
        ; Fst   -> "fst"  ; Snd  -> "snd"  }

--------------------------------------------------------------------------------
--                                   TYPES
--------------------------------------------------------------------------------

-- Type is a recursive data type used for representing the types of functions
data Type
    = TInt
    | TBool
    | TChar
    | TList  Type
    | TSum   Type Type
    | TProd  Type Type
    | TFunc  Type Type
    | TVar   Int            -- Type variables are numbers, not strings
    | TQuant Int  Type      -- Type quantifier
    | ParserTVar  String    -- The parser will parse type variables as strings.
                            -- These are to be replaced with integers by
                            -- calling convertTVars before the expressions
                            -- are used.
    deriving (Eq, Ord)

instance Show Type where
    show t = case t of
        -- Extra rule to capture the right-associativity of the function type
        -- arrow throught bracketing
        TFunc (TFunc a b) c ->
            '(' : show a ++ " -> " ++ show b ++ ") -> " ++ show c

        TInt         -> "Int"
        TBool        -> "Bool"
        TChar        -> "Char"
        TFunc a b    -> show a ++ " -> " ++ show b
        TList a      -> '[' : show a ++ "]"
        TSum  a b    -> "{ " ++ show a ++ " | " ++ show b ++ " }"
        TProd a b    -> "{ " ++ show a ++ " & " ++ show b ++ " }"
        TVar varNo   -> "_T" ++ show varNo ++ "_"
        TQuant i t   -> 'V' : (show i) ++ '.' : (show t)
        ParserTVar s -> s


--------------------------------------------------------------------------------
--                            FUNCTIONS OVER TERMS
--------------------------------------------------------------------------------

-- Returns a list of the subexpressions of the given tree node
subs :: Term -> [Term]
subs (Abs _ _ x)  = [x]
subs (AbsInf _ x) = [x]
subs (App x y)    = [x, y]
subs _            = []

-- Creates a list of all name occurrences in an Term - If a name is used twice,
-- it will appear twice in the returned list, etc.
names :: Term -> [Name]
names (Abs n _ x)  = n : names x
names (AbsInf n x) = n : names x
names (Var n)      = [n]
names (Constant _) = []
names exp          = concat (map names (subs exp))

-- Returns true if there are any occurrences (free or bound) of the given string
-- as a name in the given Term. Used by newName to generate names that are not
-- found in a given expression.
nameIn :: Name -> Term -> Bool
nameIn n (Abs n' _ x)  = (n == n') || nameIn n x
nameIn n (AbsInf n' x) = (n == n') || nameIn n x
nameIn n (Var n')      = n == n'
nameIn _ (Constant _)  = False
nameIn n exp           = any (nameIn n) (subs exp)

-- Creates a set of all free variables in a Term, which can be used when
-- reducing a function application, as it determines which names will need to be
-- changed in order to prevent name clashes.
freeVars :: Term -> S.Set Name
freeVars exp = case exp of
    Var x      -> S.singleton x
    Abs x _ m  -> S.delete x (freeVars m)
    AbsInf x m -> S.delete x (freeVars m)
    Constant _ -> S.empty
    _          -> S.unions $ map freeVars $ subs exp

-- Composes S.toList with freeVars, yielding a list of the free names in a
-- Term, as opposed to a Set
freeNames :: Term -> [Name]
freeNames = S.toList . freeVars

maxTVarInExp :: Term -> Int 
maxTVarInExp exp = maximum (0 : map maxTVar typesInExp)
    where typesInExp = getTypesInExp exp

-- Get a list containing all types that occur within an expression
getTypesInExp :: Term -> [Type]
getTypesInExp (Abs _ t m) = t : getTypesInExp m
getTypesInExp exp         = concat (map getTypesInExp (subs exp))

--------------------------------------------------------------------------------
--                            FUNCTIONS OVER TYPES
--------------------------------------------------------------------------------

-- Get a list of all type variable names in a type. The order of the list
-- the order in which those type variables appear in the type.
tVarNames :: Type -> [Int]
tVarNames (TQuant x t)   = x : tVarNames t
tVarNames (TVar x)       = [x]
tVarNames (TFunc t1 t2)  = tVarNames t1 ++ tVarNames t2
tVarNames (TProd t1 t2)  = tVarNames t1 ++ tVarNames t2
tVarNames (TSum  t1 t2)  = tVarNames t1 ++ tVarNames t2
tVarNames (TList t)      = tVarNames t
tVarNames (ParserTVar _) = error ("tVarNames: ParserTVar found")
tVarNames _              = []

-- Converts all occurrences of a particular TVar name to a different name within
-- a type
renameType :: Int -> Int -> Type -> Type
renameType from to (TQuant x t)   =
    TQuant (if x == from then to else x) (renameType from to t)
renameType from to (TVar x)       = TVar (if x == from then to else x)
renameType from to (TFunc t1 t2)  = TFunc renamedT1 renamedT2
    where renamedT1 = renameType from to t1
          renamedT2 = renameType from to t2
renameType from to (TProd t1 t2)  = TProd renamedT1 renamedT2
    where renamedT1 = renameType from to t1
          renamedT2 = renameType from to t2
renameType from to (TSum t1 t2)   = TSum renamedT1 renamedT2
    where renamedT1 = renameType from to t1
          renamedT2 = renameType from to t2
renameType from to (TList t)      = TList (renameType from to t)
renameType _    _  (ParserTVar _) = error ("renameType: ParserTVar found")
renameType _    _  other          = other

-- Rename all occurrences of all the types in the given list to another type.
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
