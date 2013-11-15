-- This module provides some definitions for commonly used (simply typed) lambda
-- terms, such as conditionals etc.
module TypedTerms where

import TypedSyntax

_idInt_   = Abs "x" (TFunc TInt  TInt)  (Var "x")
_idBool_  = Abs "x" (TFunc TBool TBool) (Var "x")
_idChar_  = Abs "x" (TFunc TChar TChar) (Var "x")
_idInt_   = Abs "x" (TFunc TInt  TInt)  (Var "x")

-- Function that generates church numeral natural numbers
lamInt :: Int -> Expression
lamInt n = Abs "f" (Abs "x" (nApps n))
    where nApps :: Int -> TypedExp
          nApps n | n  < 0 = error "Cannot generate negative number"
                  | n == 0 = Var "x"
                  | n  > 0 = App (Var "f") (nApps (n - 1))

-- Arithmetic operations
suc = Abs "n" (Abs "f" (Abs "x" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x")))))
plu = Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (App (Var "m") (Var "f")) (App (App (Var "n") (Var "f")) (Var "x"))))))
pow = Abs "b" (Abs "e" (App (Var "e") (Var "b")))

yco = App (Abs "g" (Abs "x" (App (Var "g") (App (Var "x") (Var "x"))))) (Abs "x" (App (Var "g") (App (Var "x") (Var "x"))))
