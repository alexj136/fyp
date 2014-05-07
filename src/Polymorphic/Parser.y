{
module Parser where

import Lexer
import Syntax
import PostParsing

{-- CONTEXT FREE GRAMMAR

PROG ::= type ID = TY
       | def ID ARGS = EXP
       | epsilon
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
STR  ::= ".*"

--}
}

%name parse     PARSERESULT
%name parseExp  EXP
%name parseType TY

%tokentype { Token }
%error { parseError }

%nonassoc TyInt TyBool TyChar TyStr TySum TyProd
%nonassoc IsZero Not RemL RemR Fst Snd Head Tail Null
%nonassoc ClosBr ClosSq ClosCr OpenBr OpenSq OpenCr

%nonassoc Lam Dot Let Equals In If Then Else Int Bool Str
%nonassoc IdLC IdUC

%right Xor
%right Or
%right And
%nonassoc EqEq LsEq Less GtEq Grtr NoEq

%nonassoc Sub
%right Add
%nonassoc Mod
%nonassoc Div
%right Mul

%right TyArrw Cons

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
    UndrSc  { TokenUndrSc p    }

    Def     { TokenDef    p    }
    Type    { TokenType   p    }
    
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
    IsLeft  { TokenIsLeft p    }
    Fst     { TokenFst    p    }
    Snd     { TokenSnd    p    }
    Head    { TokenHead   p    }
    Tail    { TokenTail   p    }
    Null    { TokenNull   p    }
    Cons    { TokenCons   p    }

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

PARSERESULT :: { ParseResult }
PARSERESULT : Type IdLC Equals TY      PARSERESULT { addTyDec $5 ($2, $4)            }
            | Def IdLC ARGS Equals EXP PARSERESULT { addFuncPR $6 (FuncInf $2 $3 $5) }
            | {- empty -}                          { emptyPR                         }

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
   | ID                         { ParserTVar $1 }

EXP :: { Term }
EXP : Let IdLC ARGS Equals EXP In EXP { App (AbsInf $2 $7) (toLambdas (FuncInf $2 $3 $5)) }
    | Lambda IdLC Dot EXP             { AbsInf $2 $4                              }
    | EXP EXP              %prec app  { App $1 $2                                 }
    | IdLC                            { Var $1                                    }
    | If EXP Then EXP Else EXP        { App (App (App (Operation Cond) $2) $4) $6 }
    | EXP INFIXBINOP EXP              { App (App $2 $1) $3                        }
    | IsZero                          { Operation IsZ                             }
    | Not                             { Operation Not                             }
    | RemL                            { Operation RemL                            }
    | RemR                            { Operation RemR                            }
    | IsLeft                          { Operation IsLeft                          }
    | Fst                             { Operation Fst                             }
    | Snd                             { Operation Snd                             }
    | Head                            { Operation Head                            }
    | Tail                            { Operation Tail                            }
    | Null                            { Operation Null                            }
    | LIST                            { $1                                        }
    | OpenBr EXP ClosBr               { $2                                        }
    | OpenCr EXP Comma UndrSc ClosCr  { App (Operation InjL) $2                   }
    | OpenCr UndrSc Comma EXP ClosCr  { App (Operation InjR) $4                   }
    | OpenCr EXP Comma EXP ClosCr     { App (App (Operation Tuple) $2) $4         }
    | Int                             { Constant (IntVal $1)                      }
    | Bool                            { Constant (BoolVal $1)                     }
    | Str                             { parseStr $1                               }

LIST :: { Term }
LIST : OpenSq ClosSq       { Operation Empty                  }
     | OpenSq EXP LISTREST { App (App (Operation Cons) $2) $3 }

LISTREST :: { Term }
LISTREST : Comma EXP LISTREST { App (App (Operation Cons) $2) $3 }
         | ClosSq             { Operation Empty                  }

INFIXBINOP :: { Term }
INFIXBINOP : Add  { Operation Add  }
           | Sub  { Operation Sub  }
           | Mul  { Operation Mul  }
           | Div  { Operation Div  }
           | Mod  { Operation Mod  }

           | And  { Operation And  }
           | Or   { Operation Or   }
           | Xor  { Operation Xor  }

           | Less { Operation Lss  }
           | LsEq { Operation LsE  }
           | EqEq { Operation Equ  }
           | NoEq { Operation NEq  }
           | Grtr { Operation Gtr  }
           | GtEq { Operation GtE  }

           | Cons { Operation Cons }

ID :: { String }
ID : IdLC { $1 }
   | IdUC { $1 }
{
parseStr :: String -> Term
parseStr []     = Operation Empty
parseStr (c:cs) =
    App (App (Operation Cons) (Constant (CharVal c))) (parseStr cs)

parseError :: [Token] -> a
parseError []     = error "Reached end of file while parsing"
parseError (t:ts) = error ("Parse error on line " ++ show (getY t) ++
                           ", column " ++ show (getX t) ++ ".")
}
