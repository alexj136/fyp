module TypeReconstructer where

import PolymorphicSyntax
import qualified Data.Map as M
import qualified Data.Set as S

data TypeCheckResult = Pass TypedExp | Fail TypedExp String

type Context = M.Map Name Type

type Constraint = (Type, Type)

freshTVar :: S.Set Constraint -> Int
freshTVar constr  = freshTVar' 0 constr
    where
        freshTVar' :: Int -> S.Set Constraint -> Int
        freshTVar' x constr | hasTVar x constr = freshTVar' (x + 1) constr
                            | otherwise        = x

        hasTVar :: Int -> S.Set Constraint -> Bool
        hasTVar _ S.empty = False
        hasTVar x constr
            | typeHasTVar x t1 || typeHasTVar x t2 = True
            | otherwise                            = hasTVar x rest

        ((t1, t2), rest) = breakSet constr

        typeHasTVar :: Int -> Type -> Bool
        typeHasTVar x (TFunc t1 t2) = typeHasTVar x t1 || typeHasTVar x t2
        typeHasTVar x (TList t)     = typeHasTVar x t
        typeHasTVar x (TVar y)      = x == y
        typeHasTVar _ _             = False

        breakSet :: S.Set a -> (a, S.Set a)
        breakSet S.empty = error "hasTVar: cannot break an empty set"
        breakSet s       = (sf, S.delete sf s) where sf = S.findMin s
{--
getConstraints :: Context -> TypedExp -> S.Set Constraint -> S.Set Constraint
getConstraints ctx exp constr = case exp of
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
