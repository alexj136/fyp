module Unifier where

import Syntax
import Interpreter (replace, preventClashes)
import qualified Data.Map as M
import qualified Data.Set as S

-- Do type inference on an expression - get the constraints & type, unify the
-- constraints, and use the function returned by unify to rewrite the type
-- without type variables. The ConstraintSet parameter is the set of initial
-- constraints i.e. the user's type aliases
infer :: Term -> Maybe Type
infer exp = fmap fst (inferFull M.empty exp)

-- Infer the type of a term, with type aliases. Both the term & aliases should
-- not contain ParserTVars. Return the constraint set with the inferred type.
inferFull :: Context -> Term -> Maybe (Type, ConstraintSet)
inferFull initialCtx exp = do
    rewrite <- unify constraints
    return (rewrite typeOfExp, constraints)
  where
    (constraints, typeOfExp, i) =
        getConstraints (maxTVarInExp exp + 1) initialCtx exp

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

        -- Check that we don't have ParserTVars - these should have been removed
        -- before being passed to the unifier
        | isParserTVar t1 || isParserTVar t2 = error ("unify: ParserTVars " ++
            "present in constraint set")

        -- If the types are already equal, unify the rest of the set
        | t1 == t2 = unify rest

        -- If t1 is a TVar, not occurring in t2 (i.e. if the type is not
        -- infinite), replace all occurences of t1 with t2 in the remaining
        -- constraints, and try to unify those
        | isTVar t1 && (not $ occurs (numOfTVar t1) t2) = do
            unifyRest <- unify (constrSetMap (typeSubst (t1, t2)) rest)
            return $ unifyRest . typeSubst (t1, t2)

        -- If t2 is a TVar, not occurring in t1 (i.e. if the type is not
        -- infinite), replace all occurences of t2 with t1 in the remaining
        -- constraints, and try to unify those
        | isTVar t2 && (not $ occurs (numOfTVar t2) t1) = do
            unifyRest <- unify (constrSetMap (typeSubst (t2, t1)) rest)
            return $ unifyRest . typeSubst (t2, t1)

        -- If both t1 and t2 are function types, try to unify the argument type
        -- of t1 with the argument type of t2, and the return type of t1 with
        -- the return type of t2
        | isTFunc t1 && isTFunc t2 =
            unify $ S.insert ( tFuncFrom t1 , tFuncFrom t2 )
                  $ S.insert ( tFuncTo   t1 , tFuncTo   t2 ) rest

        -- If the constraints are both lists, try to unify their inner-types
        | isTList t1 && isTList t2 =
            unify $ S.insert ( tListOf t1 , tListOf t2 ) rest

        -- Apply the same logic we applied for functions and lists, to sums and
        -- products - try to match the inner-types.
        | isTSum t1 && isTSum t2 =
            unify $ S.insert ( tSumL t1 , tSumL t2 )
                  $ S.insert ( tSumR t1 , tSumR t2 ) rest
        | isTProd t1 && isTProd t2 =
            unify $ S.insert ( tProdFst t1 , tProdFst t2 )
                  $ S.insert ( tProdSnd t1 , tProdSnd t2 ) rest

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
        TSum  t1 t2     -> TSum  (typeSubst' t1) (typeSubst' t2)
        TProd t1 t2     -> TProd (typeSubst' t1) (typeSubst' t2)
        basicType       -> basicType
        where typeSubst' = typeSubst (TVar x, tArg)
    typeSubst _ _ = error "typeSubst: only TVars can be substituted"

    -- Checks if a given type variable (TVars are integers) occurs in a
    -- given type expression
    occurs :: Int -> Type -> Bool
    occurs x (TVar y)      = x == y
    occurs x (TFunc t1 t2) = occurs x t1 || occurs x t2
    occurs x (TList t)     = occurs x t
    occurs x (TSum  t1 t2) = occurs x t1 || occurs x t2
    occurs x (TProd t1 t2) = occurs x t1 || occurs x t2
    occurs _ _             = False

    -- Determine if a type is a type variable
    isTVar :: Type -> Bool
    isTVar (TVar _) = True
    isTVar _        = False

    -- Determine if a type is a ParserTVar - if it is, we've encountered an
    -- error
    isParserTVar :: Type -> Bool
    isParserTVar (ParserTVar _) = True
    isParserTVar _              = False

    -- Retrieve the type variable number from a type variable
    numOfTVar :: Type -> Int
    numOfTVar (TVar x) = x
    numOfTVar _        = error "numOfTVar: Cannot get name of non-TVar Type"

    -- Determine if a type is a function type
    isTFunc :: Type -> Bool
    isTFunc (TFunc _ _) = True
    isTFunc _           = False

    -- Retrieve the subtypes of function types
    tFuncFrom, tFuncTo :: Type -> Type
    tFuncFrom (TFunc x _) = x
    tFuncFrom _           = error ("tFuncFrom: Cannot get inner type of " ++
                                   "non-function type")
    tFuncTo   (TFunc _ x) = x
    tFuncTo   _           = error ("tFuncTo: Cannot get inner type of " ++
                                   "non-function type")

    -- Determine if a type is a list type
    isTList :: Type -> Bool
    isTList (TList _) = True
    isTList _         = False

    -- Retrieve the inner type of a list type
    tListOf :: Type -> Type
    tListOf (TList x) = x
    tListOf _         = error "tListOf: Cannot get inner type of non-list type"

    -- Determine if a type is a sum type
    isTSum :: Type -> Bool
    isTSum (TSum _ _) = True
    isTSum _          = False

    -- Retrieve the inner types of sum types
    tSumL, tSumR :: Type -> Type
    tSumL (TSum x _) = x
    tSumL _          = error "tSumL: Cannot get inner type of non-sum type"
    tSumR (TSum _ x) = x
    tSumR _          = error "tSumR: Cannot get inner type of non-sum type"

    -- Determine if a type is a product type
    isTProd :: Type -> Bool
    isTProd (TProd _ _) = True
    isTProd _           = False

    -- Retrieve the inner types of product types
    tProdFst, tProdSnd :: Type -> Type
    tProdFst (TProd x _) = x
    tProdFst _           = error ("tProdFst: Cannot get inner type of " ++
                                  "non-product type")
    tProdSnd (TProd _ x) = x
    tProdSnd _           = error ("tProdSnd: Cannot get inner type of " ++
                                  "non-product type")

--------------------------------------------------------------------------------
--                           CONSTRAINT GENERATION
--------------------------------------------------------------------------------

-- The context type used to represent context. A context is a map that maps
-- variable names to their types
type Context = M.Map Name Type

-- Look up the type of a variable from a Context
typeFromContext :: Context -> Name -> Type
typeFromContext ctx n = case M.lookup n ctx of
    Just t  -> t
    Nothing ->
        error ("getConstraints: No binding for variable '" ++ n ++
                "' in context query")

-- Add the given binding to a Context
addToContext :: Name -> Type -> Context -> Context
addToContext = M.insert

-- An initial context to provide to getConstraints when typing a Prog object -
-- this allows mutually recursive functions to be typeable. The Int parameter
-- works in the same way as with many functions that deal with type variables.
-- It represents the next available type variable number that can be used to
-- introduce a new type variable. When used, it must be incremented for the next
-- place in which it will be used. As such it must be returned, not just passed
-- down to recursive calls.
contextProg :: Int -> Prog -> (Int, Context)
contextProg i (Prog pgMap)
    | M.null pgMap = (i, M.empty)
    | otherwise    =
        let ((_, f), rest) = M.deleteFindMin pgMap in
            case f of
                Func ty nm _ _ -> (i', ctxAll)
                    where (i', ctxRest) = contextProg i (Prog rest)
                          ctxAll = addToContext nm ty ctxRest
                FuncInf nm _ _ -> (i' + 1, ctxAll)
                    where (i', ctxRest) = contextProg i (Prog rest)
                          ctxAll = addToContext nm (TVar i') ctxRest

-- Get the typing constraints from an expression, that are required for type
-- unification. Prevents type variable name clashes by taking as a parameter the
-- lowest number that is safe to use as a type variable, and returning in the
-- tuple the lowest type variable that it would be safe to introduce, so that
-- subsequent calls can avoid using the same names (names are integers)
getConstraints :: Int -> Context -> Term -> (ConstraintSet, Type, Int)
getConstraints i ctx exp = case exp of

    -- Let-in expressions: here we perform a single step of evaluation before we
    -- generate constraints, such that we are able to have let-polymorphism,
    -- where we can use a function on arguments with different types, without
    -- making the program untypable.
    --App (Abs    v _ m) n -> getConstraints i ctx (App (AbsInf v m) n)
    App (Abs    v t m) n
        | not (containsParserTVar t) ->
            getConstraints i ctx (App (AbsInf v m) n)
    App (AbsInf v   m) n -> (S.union constrApp constrN, tApp, i'')
      where
        (constrN  , tN  , i' ) = getConstraints i ctx n
        (constrApp, tApp, i'') =
            getConstraints i' ctx (replace v (preventClashes m n) n)

    -- Explicitly declared abstractions are ignored with let-polymorphism. We
    -- must choose a new type variable for each application of our function in
    -- order for it to be typable. The original typing rule for explicitly
    -- declared abstractions is show as a comment below. We could use this rule
    -- if we did not want let-polymorphism.
    --Abs v t m -> getConstraints i ctx (AbsInf v m)
    Abs v t m
        | not (containsParserTVar t) -> getConstraints i ctx (AbsInf v m)
        | otherwise                  -> (constrM, TFunc convTy tM, i'')
            where
                (constrM, tM, i'') = getConstraints i' ctx' m
                ctx' = addToContext v t ctx
                (i', _, convTy) = convertTVarsType (i, M.empty, t)

    {- ORIGINAL RULE FOR EXPLICITLY TYPED ABSTRACTIONS
    Abs v t m -> (constrM, TFunc t tM, i')
      where
        (constrM, tM, i') = getConstraints i ctx' m
        ctx' = addToContext v t ctx
    -}

    -- Undeclared abstractions - inferred via introduction of a type variable
    AbsInf v m -> (constrM, TFunc (TVar i') tM, i' + 1)
      where
        (constrM, tM, i') = getConstraints i ctx' m
        ctx' = addToContext v (TVar i') ctx

    -- Variables
    Var v | not (containsParserTVar (typeFromContext ctx v)) ->
        (S.empty, typeFromContext ctx v, i)
          | otherwise ->
        (S.empty, convTy, i')
          where
            (i', _, convTy) =
                convertTVarsType (i, M.empty, (typeFromContext ctx v))

    -- Function application
    App m n -> (constr', tX, i'' + 1)
      where
        constr' = S.unions [constrM, constrN, S.singleton (tM, TFunc tN tX)]
        tX = TVar i''
        (constrM, tM, i' ) = getConstraints i  ctx m
        (constrN, tN, i'') = getConstraints i' ctx n

    -- Simple stuff
    Constant (IntVal  _) -> (S.empty, TInt , i )
    Constant (BoolVal _) -> (S.empty, TBool, i )
    Constant (CharVal _) -> (S.empty, TChar, i )
    Operation ot         -> (S.empty, opTy , i')
        where (opTy, i', _) = deQuantify i M.empty (typeOfOperation ot)

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
    NEq -> TFunc TInt  (TFunc TInt  TBool)
    Gtr -> TFunc TInt  (TFunc TInt  TBool)
    GtE -> TFunc TInt  (TFunc TInt  TBool)

    Not -> TFunc TBool TBool
    IsZ -> TFunc TInt  TBool

    Equ -> TQuant 0 (TFunc (TVar 0) (TFunc (TVar 0) TBool))

    Empty -> TQuant 0 (TList (TVar 0))
    Cons  -> TQuant 0 (TFunc (TVar 0) (TFunc (TList (TVar 0)) (TList (TVar 0))))
    Null  -> TQuant 0 (TFunc (TList (TVar 0)) TBool)
    Head  -> TQuant 0 (TFunc (TList (TVar 0)) (TVar 0))
    Tail  -> TQuant 0 (TFunc (TList (TVar 0)) (TList (TVar 0)))

    Cond  -> TQuant 0 (TFunc (TBool) (TFunc (TVar 0) (TFunc (TVar 0) (TVar 0))))

    Fix   -> TQuant 0 (TFunc (TFunc (TVar 0) (TVar 0)) (TVar 0))

    InjL   -> TQuant 0 (TQuant 1 (TFunc (TVar 0) (TSum (TVar 0) (TVar 1))))
    InjR   -> TQuant 0 (TQuant 1 (TFunc (TVar 1) (TSum (TVar 0) (TVar 1))))
    RemL   -> TQuant 0 (TQuant 1 (TFunc (TSum (TVar 0) (TVar 1)) (TVar 0)))
    RemR   -> TQuant 0 (TQuant 1 (TFunc (TSum (TVar 0) (TVar 1)) (TVar 1)))
    IsLeft -> TQuant 0 (TQuant 1 (TFunc (TSum (TVar 0) (TVar 1)) TBool))

    Tuple  -> TQuant 0 (TQuant 1 (
                 TFunc (TVar 0) (TFunc (TVar 1) (TProd (TVar 0) (TVar 1)))
             ))
    Fst    -> TQuant 0 (TQuant 1 (TFunc (TProd (TVar 0) (TVar 1)) (TVar 0)))
    Snd    -> TQuant 0 (TQuant 1 (TFunc (TProd (TVar 0) (TVar 1)) (TVar 1)))

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
    TSum  a b  -> ( TSum   a' b' , i'' , m'' )
        where ( a' , i'  , m'  ) = deQuantify i  m  a
              ( b' , i'' , m'' ) = deQuantify i' m' b
    TProd a b  -> ( TProd  a' b' , i'' , m'' )
        where ( a' , i'  , m'  ) = deQuantify i  m  a
              ( b' , i'' , m'' ) = deQuantify i' m' b
    ParserTVar _ -> error "deQuantify: ParserTVar found"

-- Determine if a type contains a ParserTVar or not
containsParserTVar :: Type -> Bool
containsParserTVar ty = case ty of
    ParserTVar _ -> True
    TFunc  a b   -> (containsParserTVar a) || (containsParserTVar b)
    TQuant _ a   -> containsParserTVar a
    TList  a     -> containsParserTVar a
    TSum   a b   -> (containsParserTVar a) || (containsParserTVar b)
    TProd  a b   -> (containsParserTVar a) || (containsParserTVar b)
    _            -> False

-- Convert all ParserTVars into proper integer TVars. Returns a map from the
-- integer type variables to the user's string type variables that can be used
-- to print better error messages.
convertTVarsType ::
    (Int, M.Map String Int, Type) -> (Int, M.Map String Int, Type)
convertTVarsType (i, m, t) = case t of

    ParserTVar s -> case M.lookup s m of
                        Nothing -> (i + 1, M.insert s i m, TVar i)
                        Just j  -> (i    , m             , TVar j)

    TFunc t1 t2  -> (i''  , m'', TFunc t1' t2')
        where (i' , m' , t1') = convertTVarsType (i , m , t1)
              (i'', m'', t2') = convertTVarsType (i', m', t2)

    TProd t1 t2  -> (i''  , m'', TProd t1' t2')
        where (i' , m' , t1') = convertTVarsType (i , m , t1)
              (i'', m'', t2') = convertTVarsType (i', m', t2)

    TSum  t1 t2  -> (i''  , m'', TSum  t1' t2')
        where (i' , m' , t1') = convertTVarsType (i , m , t1)
              (i'', m'', t2') = convertTVarsType (i', m', t2)

    TList t      -> (i'   , m' , TList t'     )
        where (i' , m' , t' ) = convertTVarsType (i , m , t )

    TQuant _ _   -> error "convertTVars: TQuant found in parsed type"
    TVar   _     -> error "convertTVars: Integer TVar found in parsed type"
    other        -> (i, m, other)
