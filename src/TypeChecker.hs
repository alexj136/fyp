module TypeChecker where

import TypedSyntax
import qualified CoreSyntax as C

import qualified Data.Map as M

data TypeCheckResult = Pass TypedExp | Fail TypedExp String

type Context = M.Map Name Type

-- Look up the type of a variable from a Context
typeFromContext :: Context -> Name -> Type
typeFromContext ctx n = case M.lookup n ctx of
    Nothing -> error ("No binding for variable: " ++ n ++ " in context query")
    Just t  -> t

-- Add the given binding to a Context
addToContext :: Name -> Type -> Context -> Context
addToContext = M.insert

typeOf :: Context -> TypedExp -> Type
typeOf ctx exp = case exp of
    Abs v t m -> TFunc t (typeOf ctx' m)
        where ctx' = addToContext v t ctx
    Var v -> typeFromContext ctx v
    App m n -> case tM of
        TFunc a b | tN == a -> b
        _                   -> error $ concat ["Error: Cannot apply '",
                                               show tM, "' to '", show tN, "'."]
        where tM = typeOf ctx m
              tN = typeOf ctx n
    Constant val -> case val of
        IntVal   _ -> TInt
        CharVal  _ -> TChar
        FloatVal _ -> TFloat
        BoolVal  _ -> TBool
    BinaryOp op m n -> binaryOpType ctx op m n

binaryOpType :: Context -> BOp -> TypedExp -> TypedExp -> Type
binaryOpType ctx op m n 
    | cAOp && tM == TInt   && tN == TInt   = TFunc TInt   (TFunc TInt   TInt)
    | cAOp && tM == TFloat && tN == TFloat = TFunc TFloat (TFunc TFloat TFloat)
    | cAOp && tM == TInt   && tN == TFloat = TFunc TInt   (TFunc TFloat TFloat)
    | cAOp && tM == TFloat && tN == TInt   = TFunc TFloat (TFunc TInt   TFloat)
    | otherwise = error "BinaryOp type not yet implemented"
    where cAOp = op == Add || op == Sub || op == Mul -- cAOp = Closed Arithmetic Op
          tM = typeOf ctx m
          tN = typeOf ctx n

discardTypes :: TypeCheckResult -> C.Expression
discardTypes (Fail _ s) = error $ "discardTypes failed: " ++ s
discardTypes (Pass exp) = case exp of
    _ -> error "discardTypes not yet implemented"
