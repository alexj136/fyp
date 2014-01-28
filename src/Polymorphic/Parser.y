{
module Parser where

import Lexer
import PolymorphicSyntax

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
%left exp

%token
    lam     { TokenLambda  }
    dot     { TokenDot     }
    openbr  { TokenOpenBr  }
    closbr  { TokenClosBr  }
    equals  { TokenEquals  }
    identLC { TokenIdLC $$ }
    identUC { TokenIdUC $$ }
    int     { TokenInt  $$ }
    bool    { TokenBool $$ }

%%

decls :: { [Decl] }
decls : decl decls        { $1 : $2 }
      | {- empty -}       { [] }

decl :: { Decl }
decl : identLC arglist equals exp { Decl $1 $2 $4 }

arglist :: { [String] }
arglist : identLC arglist { $1 : $2 }
        | {- empty -}     { [] }

exp :: { TypedExp }
exp : exp exp             { App $1 $2             } 
    | openbr exp closbr   { $2                    }
    | lam identLC dot exp { Abs $2 TNone $4       }
    | identLC             { Var $1                }
    | int                 { Constant (IntVal $1)  }
    | bool                { Constant (BoolVal $1) }

{
data Decl = Decl {
    name :: String,
    args :: [String],
    body :: TypedExp
} deriving (Show, Eq)

parseError :: [Token] -> a
parseError token = error $ "Parse error on " ++ (show token)
}
