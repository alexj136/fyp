module TypeReconstructer where

import PolymorphicSyntax
import qualified Data.Map as M
import qualified Data.Set as S

data TypeCheckResult = Pass TypedExp | Fail TypedExp String

type Context = M.Map Name Type

type ConstraintSet = S.Set (Type, Type)

{--
-- Given a set of constraints, generate a fresh type variable that does not
-- already exist in that set
freshTVar :: ConstraintSet -> Int
freshTVar constr  = freshTVar' 0 constr
    where
        freshTVar' :: Int -> ConstraintSet -> Int
        freshTVar' x constr | hasTVar x constr = freshTVar' (x + 1) constr
                            | otherwise        = x

        hasTVar :: Int -> ConstraintSet -> Bool
        hasTVar x constr
            | S.size constr == 0                   = False
            | typeHasTVar x t1 || typeHasTVar x t2 = True
            | otherwise                            = hasTVar x rest

        ((t1, t2), rest) = breakSet constr

        typeHasTVar :: Int -> Type -> Bool
        typeHasTVar x (TFunc t1 t2) = typeHasTVar x t1 || typeHasTVar x t2
        typeHasTVar x (TList t)     = typeHasTVar x t
        typeHasTVar x (TVar y)      = x == y
        typeHasTVar _ _             = False
--}

-- Type unification algorithm to calculate solutions to constraint sets. Builds
-- a new constraint set from the given one. The new set will not contain any of
-- the type variables introduced by the constraint generation algorithm,
-- assuming that the given program was typeable.
unify :: ConstraintSet -> Maybe ConstraintSet
unify c | S.size c == 0 = Just S.empty

        -- If the types are already equal, unify the rest of the set
        | t1 == t2 = unify rest

        -- If t1 is a TVar, not occuring in t2, ...
        | isTVar t1 && (not $ occurs (numOfTVar t1) t2) =
            unify $ replaceTypes t1 {-- with --} t2 {-- in --} rest -- .[t1->t2]

        -- If t2 is a TVar, not occuring in t1, ...
        | isTVar t2 && (not $ occurs (numOfTVar t2) t1) =
            unify $ replaceTypes t2 {-- with --} t1 {-- in --} rest -- .[t2->t1]

        -- If both t1 and t2 are function types, set the from-types and the
        -- to-types of each type as equal in the constraint set, and unify that
        -- constraint set
        | isTFunc t1 && isTFunc t2 =
            unify $ S.insert ( tFuncFrom t1 , tFuncFrom t2 )
                  $ S.insert ( tFuncTo   t1 , tFuncTo   t2 ) rest

        -- Otherwise, there are no rules we can apply, so fail
        | otherwise = Nothing

    where

        ((t1, t2), rest) = breakSet c

        -- Remove an element from a set, returning the element and the rest of
        -- the set, together in a tuple
        breakSet :: Ord a => S.Set a -> (a, S.Set a)
        breakSet s | S.size s == 0 = error "Cannot break an empty set"
                   | otherwise     = (sf, S.delete sf s) where sf = S.findMin s

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
        tFuncFrom _           = error "Cannot get subtype of function type"
        tFuncTo   (TFunc _ x) = x
        tFuncTo   _           = error "Cannot get subtype of function type"

-- Get the typing constraints from an expression, that are required for type
-- unification. Prevents type variable name clashes by taking as a parameter the
-- lowest number that is safe to use as a type variable, and returning in the
-- tuple the highest type variable that it consequently uses, so that subsequent
-- calls can avoid using the same names (names are integers)
getConstraints :: Int -> Context -> TypedExp -> (ConstraintSet, Type, Int)
getConstraints i ctx exp = case exp of
    Abs v t m            -> (constrM, TFunc (TVar i') tM, i' + 1)
        where (constrM, tM, i') = getConstraints i ctx' m
              ctx' = addToContext v t ctx
    Var v                -> (S.empty, typeFromContext ctx v, i)
    App m n              -> (constr', tX, i'' + 1)
        where constr' = S.unions [consM, consN, S.singleton (tM, TFunc tN tX)]
              tX = TVar i''
              (consM, tM, i' ) = getConstraints i  ctx m
              (consN, tN, i'') = getConstraints i' ctx n
    Constant (IntVal  _) -> (S.empty, TInt , i)
    Constant (BoolVal _) -> (S.empty, TBool, i)
    BinaryOp t           -> (S.empty, typeOfBinaryOp t, i)
    UnaryOp  t           -> (S.empty, typeOfUnaryOp  t, i)

{--
constr ( M N )
    let ( tM, cM ) = constr ( M ) in
    let ( tN, cN ) = constr ( N ) in
    let tB = newTypeVar() in
    ( tB, cM U cN U ( tM = tN -> tB ) )
--}

-- Look up the type of a variable from a Context
typeFromContext :: Context -> Name -> Type
typeFromContext ctx n = case M.lookup n ctx of
    Nothing -> error ("No binding for variable '" ++ n ++ "' in context query")
    Just t  -> t

-- Add the given binding to a Context
addToContext :: Name -> Type -> Context -> Context
addToContext = M.insert

-- Creates a Context from the given Name & Type as a 'singleton' Context
contextFrom :: Name -> Type -> Context
contextFrom n t = M.fromList[(n, t)]

-- Recursively infer the type of an expression
typeOf :: Context -> TypedExp -> Type
typeOf ctx exp = case exp of
    Abs v t m -> TFunc t (typeOf ctx' m)
        where ctx' = addToContext v t ctx
    Var v -> typeFromContext ctx v
    App m n -> case tM of
        TFunc a b | tN == a -> b
        _ -> error $ concat ["Error: Cannot apply '",
                    show tM, "' to '", show tN, "'."]
        where tM = typeOf ctx m
              tN = typeOf ctx n
    Constant (IntVal  _) -> TInt
    Constant (BoolVal _) -> TBool
    BinaryOp t -> typeOfBinaryOp t
    UnaryOp  t -> typeOfUnaryOp  t

typeOfBinaryOp :: BinaryOpType -> Type
typeOfBinaryOp t = case t of
    Add -> TFunc TInt  (TFunc TInt  TInt )
    Sub -> TFunc TInt  (TFunc TInt  TInt )
    Mul -> TFunc TInt  (TFunc TInt  TInt )
    Div -> TFunc TInt  (TFunc TInt  TInt )
    Mod -> TFunc TInt  (TFunc TInt  TInt )
    And -> TFunc TBool (TFunc TBool TBool)
    Or  -> TFunc TBool (TFunc TBool TBool)
    Xor -> TFunc TBool (TFunc TBool TBool)

typeOfUnaryOp :: UnaryOpType -> Type
typeOfUnaryOp IsZ = TFunc TInt  TBool
typeOfUnaryOp Not = TFunc TBool TBool
