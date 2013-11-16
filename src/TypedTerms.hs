-- This module provides some definitions for commonly used (simply typed) lambda
-- terms, such as conditionals etc.
module TypedTerms where

import qualified CoreSyntax as C
import qualified TypedSyntax as T

_idInt_   = T.Abs "x" (TFunc TInt  TInt)  (T.Var "x")
_idBool_  = T.Abs "x" (TFunc TBool TBool) (T.Var "x")
_idChar_  = T.Abs "x" (TFunc TChar TChar) (T.Var "x")
_idInt_   = T.Abs "x" (TFunc TInt  TInt)  (T.Var "x")

-- Function that generates church numeral natural numbers
lamInt :: Int -> C.Expression
lamInt n = C.Abs "f" (C.Abs "x" (nApps n))
    where nApps :: Int -> C.Expression
          nApps n | n  < 0 = error "Cannot generate negative number"
                  | n == 0 = C.Var "x"
                  | n  > 0 = C.App (C.Var "f") (nApps (n - 1))

-- Arithmetic operations
suc = C.Abs "n" (C.Abs "f" (C.Abs "x" (C.App (C.Var "f") (C.App (C.App (C.Var "n") (C.Var "f")) (C.Var "x")))))
plu = C.Abs "m" (C.Abs "n" (C.Abs "f" (C.Abs "x" (C.App (C.App (C.Var "m") (C.Var "f")) (C.App (C.App (C.Var "n") (C.Var "f")) (C.Var "x"))))))
pow = C.Abs "b" (C.Abs "e" (C.App (C.Var "e") (C.Var "b")))

yco = C.App (C.Abs "g" (C.Abs "x" (C.App (C.Var "g") (C.App (C.Var "x") (C.Var "x"))))) (C.Abs "x" (C.App (C.Var "g") (C.App (C.Var "x") (C.Var "x"))))
