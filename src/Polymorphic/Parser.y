{
module Parser where

import Lexer
import Syntax

{-- CONTEXT FREE GRAMMAR

PROG ::= PROG TYDC
       | PROG ID ARGS = EXP
       | epsilon
TYDC ::= ID : TY
TY   ::= Int | Bool | Char
       | [ TY ]
       | { TY | TY }
       | { TY & TY }
       | TY -> TY
       | ( TY )
       | ID
ARGS ::= ID ARGS
       | epsilon
EXP  ::= let ID = EXP in EXP
       | LM ID . EXP
       | EXP EXP
       | ID
       | if EXP then EXP else EXP
       | *   | +  | -   | /   | %
       | ==  | <= | >=  | /=  | <      | >
       | and | or | not | xor | iszero
       | [] | [ EXP ] | [ EXP , ... , EXP ]
       | ( EXP )
       | { _ , EXP } | { EXP , _ } | { EXP , EXP }
       | INT
LM   ::= λ | ^ | <backslash>
ID   ::= [a-z][a-zA-Z]*
INT  ::= 0 | [1-9][0-9]*
BOOL ::= true | false
CHAR ::= 'a' | 'b' | 'c' | ...
STR  ::= "*"

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
    lam     { TokenLambda  }
    dot     { TokenDot     }
    identLC { TokenIdLC $$ }
    identUC { TokenIdUC $$ }
    openbr  { TokenOpenBr  }
    closbr  { TokenClosBr  }
    opensq  { TokenOpenSq  }
    clossq  { TokenClosSq  }
    opencr  { TokenOpenCr  }
    closcr  { TokenClosCr  }
    equals  { TokenEquals  }
    comma   { TokenComma   }

    add     { TokenAdd     }
    sub     { TokenSub     }
    mul     { TokenMul     }
    div     { TokenDiv     }
    mod     { TokenMod     }

    int     { TokenInt  $$ }
    bool    { TokenBool $$ }
    

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
exp : let identLC equals exp in exp { App (AbsInf $2 $6) $4                     }
    | lam identLC dot exp           { AbsInf $2 $4                              }
    | exp exp             %prec app { App $1 $2                                 } 
    | identLC                       { Var $1                                    }
    | if exp then exp else exp      { App (App (App (Operation Cond) $2) $4) $6 }
    | exp infixBinOp exp            { App (App $2 $1) $3                        }
    | openbr exp closbr             { $2                                        }
    | int                           { Constant (IntVal $1)                      }
    | bool                          { Constant (BoolVal $1)                     }

infixBinOp :: { TypedExp }
infixBinOp : add { Operation Add }
           | sub { Operation Sub }
           | mul { Operation Mul }
           | div { Operation Div }
           | mod { Operation Mod }

           | and { Operation And }
           | or  { Operation Or  }
           | xor { Operation Xor }

           | lss { Operation Lss }
           | lse { Operation LsE }
           | equ { Operation Equ }
           | neq { Operation NEq }
           | gtr { Operation Gtr }
           | gte { Operation GtE }
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
