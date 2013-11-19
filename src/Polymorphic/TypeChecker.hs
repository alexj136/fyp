module TypeChecker where

import TypedSyntax
import qualified Data.Map as M

data TypeCheckResult = Pass TypedExp | Fail TypedExp String

type Context = M.Map Name Type

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
    Constant val -> case val of
        IntVal   _ -> TInt
        CharVal  _ -> TChar
        FloatVal _ -> TFloat
        BoolVal  _ -> TBool
    BinaryOp t -> typeOfBinaryOp t

-- Infer the type of a BinaryOp from its type and arguments
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
    _   -> error "Binary operation type not yet implemented"
