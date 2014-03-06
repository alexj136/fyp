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

%name parse     PROG
%name parseExp  EXP
%name parseType TY

%tokentype { Token }
%error { parseError }

%nonassoc ClosBr ClosSq ClosCr OpenBr OpenSq OpenCr

%right Equals Lam Dot Let In Int Bool TyArrw
%left IdLC IdUC

%right Or
%right And
%nonassoc EqEq LsEq Less GtEq Grtr NoEq

%nonassoc Sub
%right Add
%nonassoc Mod
%nonassoc Div
%right Mul

%left EXP
%left app

%token
    OpenBr  { TokenOpenBr p    }
    ClosBr  { TokenClosBr p    }
    OpenSq  { TokenOpenSq p    }
    ClosSq  { TokenClosSq p    }
    OpenCr  { TokenOpenCr p    }
    ClosCr  { TokenClosCr p    }
    Equals  { TokenEquals p    }
    Comma   { TokenComma  p    }
    Colon   { TokenColon  p    }
    UndrSc  { TokenUndrSc p    }
    
    Lambda  { TokenLambda p    }
    Dot     { TokenDot    p    }

    Add     { TokenAdd    p    }
    Sub     { TokenSub    p    }
    Mul     { TokenMul    p    }
    Div     { TokenDiv    p    }
    Mod     { TokenMod    p    }

    EqEq    { TokenEqEq   p    }
    LsEq    { TokenLsEq   p    }
    Less    { TokenLess   p    }
    GtEq    { TokenGtEq   p    }
    Grtr    { TokenGrtr   p    }
    NoEq    { TokenNoEq   p    }

    And     { TokenAnd    p    }
    Or      { TokenOr     p    }
    Not     { TokenNot    p    }
    Xor     { TokenXor    p    }
    IsZero  { TokenIsZero p    }
    
    Let     { TokenLet    p    }
    In      { TokenIn     p    }

    If      { TokenIf     p    }
    Then    { TokenThen   p    }
    Else    { TokenElse   p    }

    RemL    { TokenRemL   p    }
    RemR    { TokenRemR   p    }
    Fst     { TokenFst    p    }
    Snd     { TokenSnd    p    }

    TyInt   { TokenTyInt  p    }
    TyBool  { TokenTyBool p    }
    TyChar  { TokenTyChar p    }
    TyStr   { TokenTyStr  p    }
    TySum   { TokenTySum  p    }
    TyProd  { TokenTyProd p    }
    TyArrw  { TokenTyArrw p    }

    Int     { TokenInt    p $$ }
    Bool    { TokenBool   p $$ }
    Str     { TokenStr    p $$ }

    IdLC    { TokenIdLC   p $$ }
    IdUC    { TokenIdUC   p $$ }
%%

PROG :: { [Decl] }
PROG : TYDEC IdLC ARGS Equals EXP PROG { (makeDecl $1 $2 $3 $5) : $6 }
     | TYDEC IdLC ARGS Equals EXP      {  makeDecl $1 $2 $3 $5  : [] }

TYDEC :: { Maybe Type }
TYDEC : IdLC Colon TY { Just $3 }
      | {- empty -}   { Nothing }

ARGS :: { [String] }
ARGS : IdLC ARGS   { $1 : $2 }
     | {- empty -} { []      }

TY :: { Type }
TY : TyInt                      { TInt          }
   | TyBool                     { TBool         }
   | TyChar                     { TChar         }
   | TyStr                      { TList TChar   }
   | OpenSq TY ClosSq           { TList $2      }
   | OpenCr TY TySum TY ClosCr  { TSum $2 $4    }
   | OpenCr TY TyProd TY ClosCr { TProd $2 $4   }
   | TY TyArrw TY               { TFunc $1 $3   }
   | OpenBr TY ClosBr           { $2            }
   | IdLC                       { ParserTVar $1 }

EXP :: { TypedExp }
EXP : Let IdLC Equals EXP In EXP     { App (AbsInf $2 $6) $4                     }
    | Lambda IdLC Dot EXP            { AbsInf $2 $4                              }
    | EXP EXP          %prec app     { App $1 $2                                 }
    | IdLC                           { Var $1                                    }
    | If EXP Then EXP Else EXP       { App (App (App (Operation Cond) $2) $4) $6 }
    | EXP INFIXBINOP EXP             { App (App $2 $1) $3                        }
    | IsZero                         { Operation IsZ                             }
    | Not                            { Operation Not                             }
    | RemL                           { Operation RemL                            }
    | RemR                           { Operation RemR                            }
    | Fst                            { Operation Fst                             }
    | Snd                            { Operation Snd                             }
    | LIST                           { $1                                        }
    | OpenBr EXP ClosBr              { $2                                        }
    | OpenCr EXP Comma UndrSc ClosCr { App (Operation InjL) $2                   }
    | OpenCr UndrSc Comma EXP ClosCr { App (Operation InjR) $4                   }
    | OpenCr EXP Comma EXP ClosCr    { App (App (Operation Tuple) $2) $4         }
    | Int                            { Constant (IntVal $1)                      }
    | Bool                           { Constant (BoolVal $1)                     }
    | Str                            { parseStr $1                               }

LIST :: { TypedExp }
LIST : OpenSq ClosSq       { Operation Empty                  }
     | OpenSq EXP LISTREST { App (App (Operation Cons) $2) $3 }

LISTREST :: { TypedExp }
LISTREST : Comma EXP LISTREST { App (App (Operation Cons) $2) $3 }
         | ClosSq             { Operation Empty                  }

INFIXBINOP :: { TypedExp }
INFIXBINOP : Add { Operation Add }
           | Sub { Operation Sub }
           | Mul { Operation Mul }
           | Div { Operation Div }
           | Mod { Operation Mod }

           | And { Operation And }
           | Or  { Operation Or  }
           | Xor { Operation Xor }

           | Less { Operation Lss }
           | LsEq { Operation LsE }
           | EqEq { Operation Equ }
           | NoEq { Operation NEq }
           | Grtr { Operation Gtr }
           | GtEq { Operation GtE }
{
-- A Decl object carries data that is to be converted into a function
data Decl = Decl {          -- A standard function declaration.
    tyDec :: Maybe Type,    -- The user-specified type, which we will check, but
                            -- is not necessarily present.
    name  :: String,        -- The name of the function.
    body  :: TypedExp       -- The body of the function.
} deriving Eq

instance Show Decl where
    show (Decl t n b) = show n ++ " : " ++ show t ++ "\n" ++
                        show n ++ " = " ++ show b ++ "\n"

makeDecl :: Maybe Type -> String -> [String] -> TypedExp -> Decl
makeDecl t n []   x = Decl t n x
makeDecl t n args x = makeDecl t n (init args) (AbsInf (last args) x)

parseStr :: String -> TypedExp
parseStr []     = Operation Empty
parseStr (c:cs) =
    App (App (Operation Cons) (Constant (CharVal c))) (parseStr cs)

parseError :: [Token] -> a
parseError []     = error "Reached end of file while parsing"
parseError (t:ts) = error ("Parse error on line " ++ show (getY t) ++
                           ", column " ++ show (getX t) ++ ".")
}
