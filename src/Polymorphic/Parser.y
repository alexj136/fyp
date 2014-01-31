{
module Parser where

import Lexer
import Syntax

{-- CONTEXT FREE GRAMMAR

DECLS       := DECLS DECL
            |  ε
DECL        := FUNC_NAME ARGLIST = EXP
ARGLIST     := ARGLIST ARG
            |  ε
EXP         := EXP EXP
            |  ID
            |  INT
            |  BINARYOP
            |  UNARYOP
INT         := [1-9][0-9]*
BINARYOP    := +
            |  -
            |  *
            |  div
            |  mod
UNARYOP     := iszero
            |  not
FUNC_NAME,
ID,
ARG         := [a-z][a-zA-Z]*

--}
}

%name parse
%tokentype { Token }
%error { parseError }

%left add
%left sub
%left mul
%left div
%left mod

%right equals lam dot let in int bool
%left identLC
%left closbr
%right openbr
%left exp
%left app

%token
    identLC { TokenIdLC $$ }
    identUC { TokenIdUC $$ }
    openbr  { TokenOpenBr  }
    closbr  { TokenClosBr  }
    equals  { TokenEquals  }

    lam     { TokenLambda  }
    dot     { TokenDot     }

    int     { TokenInt  $$ }
    bool    { TokenBool $$ }
    
    add     { TokenAdd     }
    sub     { TokenSub     }
    mul     { TokenMul     }
    div     { TokenDiv     }
    mod     { TokenMod     }

    let     { TokenLet     }
    in      { TokenIn      }

%%

decls :: { [Decl] }
decls : decl decls  { $1 : $2 }
      | {- empty -} { [] }

decl :: { Decl }
decl : identLC arglist equals exp { makeDecl $1 $2 $4 }

arglist :: { [String] }
arglist : identLC arglist { $1 : $2 }
        | {- empty -}     { [] }

exp :: { TypedExp }
exp : exp exp %prec app             { App $1 $2             } 
    | openbr exp closbr             { $2                    }
    | lam identLC dot exp           { AbsInf $2 $4          }
    | let identLC equals exp in exp { App (AbsInf $2 $6) $4 }
    | identLC                       { Var $1                }
    | int                           { Constant (IntVal $1)  }
    | bool                          { Constant (BoolVal $1) }
    | exp infixop exp               { App (App $2 $1) $3    }

infixop :: { BinaryOp }
infixop : add { BinaryOp Add }
        | sub { BinaryOp Sub }
        | mul { BinaryOp Mul }
        | div { BinaryOp Div }
        | mod { BinaryOp Mod }
{
data Decl = Decl {    -- A standard function declaration
    name  :: String,  -- The name if the function
    body  :: TypedExp -- The body of the function
} | DeclWTy {         -- For use with given type declarations (not yet implemented)
    name  :: String,
    tyDec :: Type,    -- The user-specified type, which we will check
    body  :: TypedExp
} deriving (Show, Eq)

makeDecl :: String -> [String] -> TypedExp -> Decl
makeDecl n []   x = Decl n x
makeDecl n args x = makeDecl n (init args) (AbsInf (last args) x)

makeDeclWTy :: Type -> String -> [String] -> TypedExp -> Decl
makeDeclWTy t n []   x = DeclWTy n t x
makeDeclWTy t n args x = makeDeclWTy t n (init args) (AbsInf (last args) x)

parseError :: [Token] -> a
parseError token = error $ "Parse error on " ++ (show token)
}
