module Unifier where

import Syntax
import qualified Data.Map as M
import qualified Data.Set as S

-- Do type inference on an expression - get the constraints & type, unify the
-- constraints, and use the function returned by unify to rewrite the type
-- without type variables
infer :: TypedExp -> Maybe Type
infer exp = case (inferWithConstraints exp) of
    Just (ty, _) -> Just ty
    Nothing      -> Nothing

inferWithConstraints :: TypedExp -> Maybe (Type, ConstraintSet)
inferWithConstraints exp = do
    rewrite <- unify deQ'dConstraints
    return (rewrite deQ'dType, constraints)
  where
    (constraints, typeOfExp, i) = getConstraints (maxTVarInExp exp + 1) M.empty exp
    (deQ'dConstraints, i') = deQuantifyConstraintSet i constraints
    (deQ'dType, _, _) = deQuantify i' M.empty typeOfExp


--------------------------------------------------------------------------------
--                                UNIFICATION
--------------------------------------------------------------------------------

-- A constraint is a pair of types (the first is a TVar), that are supposedly
-- equivalent
type Constraint = (Type, Type)
type ConstraintSet = S.Set Constraint

-- Apply a function to every Type in a ConstraintSet - output of funtion must
-- also be a Type
constrSetMap :: (Type -> Type) -> ConstraintSet -> ConstraintSet
constrSetMap f = S.map (constrMap f)

-- Apply a function to both the Types in a Constraint - output of funtion must
-- also be a Type
constrMap :: (Type -> Type) -> Constraint-> Constraint
constrMap f (t1, t2) = (f t1, f t2)

-- Build a new ConstraintSet from two types
newConstrSet :: Type -> Type -> ConstraintSet
newConstrSet t1 t2 = S.singleton (t1, t2)

-- Type unification algorithm to calculate solutions to constraint sets. Builds
-- a function from types to types. The generated function will take a Type
-- expression, and replace all the type variables with concrete types.
unify :: ConstraintSet -> Maybe (Type -> Type)
unify c | S.size c == 0 = Just (\t -> t)

        -- If the types are already equal, unify the rest of the set
        | t1 == t2 = unify rest

        -- If t1 is a TVar, not occurring in t2, ...
        | isTVar t1 && (not $ occurs (numOfTVar t1) t2) = do
            unifyRest <- unify (constrSetMap (typeSubst (t1, t2)) rest)
            return $ unifyRest . typeSubst (t1, t2)

        -- If t2 is a TVar, not occurring in t1, ...
        | isTVar t2 && (not $ occurs (numOfTVar t2) t1) = do
            unifyRest <- unify (constrSetMap (typeSubst (t2, t1)) rest)
            return $ unifyRest . typeSubst (t2, t1)

        -- If both t1 and t2 are function types, set the argument-types and the
        -- return-types of each type as equal in the constraint set, and unify
        -- that constraint set
        | isTFunc t1 && isTFunc t2 =
            unify $ S.insert ( tFuncFrom t1 , tFuncFrom t2 )
                  $ S.insert ( tFuncTo   t1 , tFuncTo   t2 ) rest

        -- Otherwise, there are no rules we can apply, so fail
        | otherwise = Nothing
  where
    ((t1, t2), rest) = S.deleteFindMin c

    -- typeSubst takes a constraint and a type and replaces all occurences
    -- of the first type in the constraint with the second, within the given
    -- type
    typeSubst :: Constraint -> Type -> Type
    typeSubst (TVar x, tArg) tBody = case tBody of
        TVar y | x == y -> tArg
                | x /= y -> TVar y
        TFunc t1 t2     -> TFunc (typeSubst' t1) (typeSubst' t2)
        TList ty        -> TList (typeSubst' ty)
        basicType       -> basicType
        where typeSubst' = typeSubst (TVar x, tArg)
    typeSubst _ _ = error "typeSubst: only TVars can be substituted"

    -- Checks if a given type variable (TVars are integers) occurs in a
    -- given type expression
    occurs :: Int -> Type -> Bool
    occurs x (TVar y)      = x == y
    occurs x (TFunc t1 t2) = occurs x t1 || occurs x t2
    occurs x (TList t)     = occurs x t
    occurs _ _             = False

    -- Determine if a type is a type variable
    isTVar :: Type -> Bool
    isTVar (TVar _) = True
    isTVar _        = False

    -- Retrieve the type variable number from a type variable
    numOfTVar :: Type -> Int
    numOfTVar (TVar x) = x
    numOfTVar _        = error "Cannot get name of non-TVar Type"

    -- Determine if a type is a function type
    isTFunc :: Type -> Bool
    isTFunc (TFunc _ _) = True
    isTFunc _           = False

    -- Retrieve the subtypes of function types
    tFuncFrom, tFuncTo :: Type -> Type
    tFuncFrom (TFunc x _) = x
    tFuncFrom _           = error "Cannot get subtype of non-function type"
    tFuncTo   (TFunc _ x) = x
    tFuncTo   _           = error "Cannot get subtype of non-function type"

--------------------------------------------------------------------------------
--                           CONSTRAINT GENERATION
--------------------------------------------------------------------------------

-- The context type used to represent context. A context is a map that maps
-- variable names to their types
type Context = M.Map Name Type

-- Get the typing constraints from an expression, that are required for type
-- unification. Prevents type variable name clashes by taking as a parameter the
-- lowest number that is safe to use as a type variable, and returning in the
-- tuple the highest type variable that it consequently uses, so that subsequent
-- calls can avoid using the same names (names are integers)
getConstraints :: Int -> Context -> TypedExp -> (ConstraintSet, Type, Int)
getConstraints i ctx exp = case exp of

    -- Explicitly declared abstractions
    Abs v t m -> (constrM, TFunc t tM, i')
      where
        (constrM, tM, i') = getConstraints i ctx' m
        ctx' = addToContext v t ctx

    -- Undeclared abstractions - inferred via introduction of a type variable
    AbsInf v m -> (constrM, TFunc (TVar i') tM, i' + 1)
      where
        (constrM, tM, i') = getConstraints i ctx' m
        ctx' = addToContext v (TVar i') ctx

    -- Variables
    Var v -> (S.empty, typeFromContext ctx v, i)
{--
    Not needed - type quantifiers used instead
    -- List constructor
    App (App (Operation Cons) m) n -> (constr', tN, i'')
      where
        constr' = S.unions [constrM, constrN, S.singleton (tN, TList tM)]
        (constrM, tM, i' ) = getConstraints i  ctx m
        (constrN, tN, i'') = getConstraints i' ctx n
--}
    -- Function application
    App m n -> (constr', tX, i'' + 1)
      where
        constr' = S.unions [constrM, constrN, S.singleton (tM, TFunc tN tX)]
        tX = TVar i''
        (constrM, tM, i' ) = getConstraints i  ctx m
        (constrN, tN, i'') = getConstraints i' ctx n

    -- Simple stuff
    Constant (IntVal  _) -> (S.empty, TInt , i)
    Constant (BoolVal _) -> (S.empty, TBool, i)
    Constant (CharVal _) -> (S.empty, TChar, i)
    Operation ot         -> (S.empty, typeOfOperation ot, i)

  where
    -- Look up the type of a variable from a Context
    typeFromContext :: Context -> Name -> Type
    typeFromContext ctx n = case M.lookup n ctx of
        Just t  -> t
        Nothing ->
            error ("No binding for variable '" ++ n ++ "' in context query")

    -- Add the given binding to a Context
    addToContext :: Name -> Type -> Context -> Context
    addToContext = M.insert

{-- Constraint generation algorithm (application case):
constr ( M N )
    let ( tM, cM ) = constr ( M ) in
    let ( tN, cN ) = constr ( N ) in
    let tB = newTypeVar() in
    ( tB, cM U cN U ( tM = tN -> tB ) )
--}


-- Lookup the types of the various binary and unary operations
typeOfOperation :: OpType -> Type
typeOfOperation ot = case ot of
    Add -> TFunc TInt  (TFunc TInt  TInt )
    Sub -> TFunc TInt  (TFunc TInt  TInt )
    Mul -> TFunc TInt  (TFunc TInt  TInt )
    Div -> TFunc TInt  (TFunc TInt  TInt )
    Mod -> TFunc TInt  (TFunc TInt  TInt )

    And -> TFunc TBool (TFunc TBool TBool)
    Or  -> TFunc TBool (TFunc TBool TBool)
    Xor -> TFunc TBool (TFunc TBool TBool)

    Lss -> TFunc TInt  (TFunc TInt  TBool)
    LsE -> TFunc TInt  (TFunc TInt  TBool)
    Equ -> TFunc TInt  (TFunc TInt  TBool)
    NEq -> TFunc TInt  (TFunc TInt  TBool)
    Gtr -> TFunc TInt  (TFunc TInt  TBool)
    GtE -> TFunc TInt  (TFunc TInt  TBool)

    Not -> TFunc TBool TBool
    IsZ -> TFunc TInt  TBool

    Empty -> TQuant 0 (TList (TVar 0))
    Cons  -> TQuant 0 (TFunc (TVar 0) (TFunc (TList (TVar 0)) (TList (TVar 0))))
    Null  -> TQuant 0 (TFunc (TList (TVar 0)) TBool)
    Head  -> TQuant 0 (TFunc (TList (TVar 0)) (TVar 0))
    Tail  -> TQuant 0 (TFunc (TList (TVar 0)) (TList (TVar 0)))

-- Remove all type quantifiers from the constraints & type returned by the
-- constraint generation algorithm
deQuantifyConstraintSet :: Int -> ConstraintSet -> (ConstraintSet, Int)
deQuantifyConstraintSet i constr
    | S.null constr = (constr, i)
    | otherwise     = (S.insert (t1', t2') rest', i''')
        where ((t1, t2), rest) = S.deleteFindMin constr
              (t1', i' , _)    = deQuantify i  M.empty t1
              (t2', i'', _)    = deQuantify i' M.empty t2
              (rest', i''')    = deQuantifyConstraintSet i'' rest

-- Remove type quantifiers from a type expression, replacing locally bound type
-- variables with globally unique, usable ones. The Int paramerer is a lowest
-- available type variable. The Map parameter specifies which type variables
-- should be replaced and what they should be replaced with.
deQuantify :: Int -> (M.Map Int Int) -> Type -> (Type, Int, M.Map Int Int)
deQuantify i m t = case t of
    TInt       -> ( TInt        , i   , m   )
    TBool      -> ( TBool       , i   , m   )
    TChar      -> ( TChar       , i   , m   )
    TList a    -> ( TList a'    , i'  , m'  )
        where ( a' , i'  , m'  ) = deQuantify i  m  a
    TFunc a b  -> ( TFunc a' b' , i'' , m'' )
        where ( a' , i'  , m'  ) = deQuantify i  m  a
              ( b' , i'' , m'' ) = deQuantify i' m' b
    TQuant x a -> ( a'          , i'' , m'' )
        where ( a' , i'' , m'' ) = deQuantify i' m' a
              m' = M.insert x i m
              i' = i + 1
    TVar x     -> case M.lookup x m of
                  Just x' -> ( TVar x' , i , m )
                  Nothing -> ( TVar x  , i , m )
