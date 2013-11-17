module TypeChecker where

import TypedSyntax
import qualified CoreSyntax as C

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
    BinaryOp op m n -> binaryOpType ctx op m n

-- Infer the type of a BinaryOp from its type and arguments
binaryOpType :: Context -> BOp -> TypedExp -> TypedExp -> Type
binaryOpType ctx op m n =
    if opIs [Add, Sub, Mul, Div, Mod] then case (tM, tN) of
        ( TBool  , _      ) -> errMsg
        ( _      , TBool  ) -> errMsg
        ( TChar  , _      ) -> errMsg
        ( _      , TChar  ) -> errMsg
        ( TInt   , TInt   ) -> TInt   --TFunc TInt   (TFunc TInt   TInt  )
        ( TInt   , TFloat ) -> TFloat --TFunc TInt   (TFunc TFloat TFloat)
        ( TFloat , TInt   ) -> TFloat --TFunc TFloat (TFunc TInt   TFloat)
        ( TFloat , TFloat ) -> TFloat --TFunc TFloat (TFunc TFloat TFloat)
    else if opIs [] then case (tM, tN) of
        _ -> error $ "Op type: '" ++ show op ++ "' not yet implemented"
    else error $ "Op type: '" ++ show op ++ "' not yet implemented"
    where opIs = or . map (== op)
          tM = typeOf ctx m
          tN = typeOf ctx n
          errMsg = error $ concat ["Error: Cannot apply '", show op,
                         "' to '", show tM, "' and '", show tN, "'"]

-- Throw away the types from a TypedExp to obtain an untyped Expression
discardTypes :: TypeCheckResult -> C.Expression
discardTypes (Fail _ s) = error $ "discardTypes failed: " ++ s
discardTypes (Pass exp) = case exp of
    _ -> error "discardTypes not yet implemented"
