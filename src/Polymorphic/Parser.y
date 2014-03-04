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
    OpenBr  { TokenOpenBr  }
    ClosBr  { TokenClosBr  }
    OpenSq  { TokenOpenSq  }
    ClosSq  { TokenClosSq  }
    OpenCr  { TokenOpenCr  }
    ClosCr  { TokenClosCr  }
    Equals  { TokenEquals  }
    Comma   { TokenComma   }
    Colon   { TokenColon   }
    UndrSc  { TokenUndrSc  }
    
    Lambda  { TokenLambda  }
    Dot     { TokenDot     }

    Add     { TokenAdd     }
    Sub     { TokenSub     }
    Mul     { TokenMul     }
    Div     { TokenDiv     }
    Mod     { TokenMod     }

    EqEq    { TokenEqEq    }
    LsEq    { TokenLsEq    }
    Less    { TokenLess    }
    GtEq    { TokenGtEq    }
    Grtr    { TokenGrtr    }
    NoEq    { TokenNoEq    }

    And     { TokenAnd     }
    Or      { TokenOr      }
    Not     { TokenNot     }
    Xor     { TokenXor     }
    IsZero  { TokenIsZero  }
    
    Let     { TokenLet     }
    In      { TokenIn      }

    If      { TokenIf      }
    Then    { TokenThen    }
    Else    { TokenElse    }

    TyInt   { TokenTyInt   }
    TyBool  { TokenTyBool  }
    TyChar  { TokenTyChar  }
    TyStr   { TokenTyStr   }
    TySum   { TokenTySum   }
    TyProd  { TokenTyProd  }
    TyArrw  { TokenTyArrw  }

    Int     { TokenInt  $$ }
    Bool    { TokenBool $$ }
    Str     { TokenStr  $$ }

    IdLC    { TokenIdLC $$ }
    IdUC    { TokenIdUC $$ }
%%

PROG :: { [Decl] }
PROG : TYDEC IdLC ARGS Equals EXP PROG { (makeDecl $1 $2 $3 $5) : $6 }
     | TYDEC IdLC ARGS Equals EXP      {  makeDecl $1 $2 $3 $5       }

TYDEC :: { Maybe Type }
TYDEC : IdLC Colon TY { Just $3 }
      | {- empty -}   { Nothing }

ARGS :: { [String] }
ARGS : IdLC ARGS   { $1 : $2 }
     | {- empty -} { []      }

TY :: { Type }
TY : TyInt                      { TInt        }
   | TyBool                     { TBool       }
   | TyChar                     { TChar       }
   | TyStr                      { TStr        }
   | OpenSq TY ClosSq           { TList $2    }
   | OpenCr TY TySum TY ClosCr  { TSum $2 $4  }
   | OpenCr TY TyProd TY ClosCr { TProd $2 $4 }
   | TY TyArrw TY               { TFunc $1 $3 }
   | OpenBr TY ClosBr           { $2          }
   | IdLC                       { TVar $1     }

EXP :: { TypedExp }
EXP : Let IdLC Equals EXP In EXP     { App (AbsInf $2 $6) $4                     }
    | Lambda IdLC Dot EXP            { AbsInf $2 $4                              }
    | EXP EXP          %prec app     { App $1 $2                                 }
    | IdLC                           { Var $1                                    }
    | If EXP Then EXP Else EXP       { App (App (App (Operation Cond) $2) $4) $6 }
    | EXP INFIXBINOP EXP             { App (App $2 $1) $3                        }
    | IsZero                         { Operation IsZ                             }
    | Not                            { Operation Not                             }
    | LIST                           { $1                                        }
    | OpenBr EXP ClosBr              { $2                                        }
    | OpenCr EXP Comma UndrSc ClosCr { App (Operation InjL) $2                   }
    | OpenCr UndrSc Comma EXP ClosCr { App (Operation InjR) $4                   }
    | Int                            { Constant (IntVal $1)                      }
    | Bool                           { Constant (BoolVal $1)                     }
    {- TODO STRINGS -}

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
data Decl = Decl {      -- A standard function declaration.
    tyDec :: Maybe Type -- The user-specified type, which we will check, but is
                        -- not necessarily present.
    name  :: String,    -- The name of the function.
    body  :: TypedExp,  -- The body of the function.
} deriving (Show, Eq)

makeDecl :: Maybe Type -> String -> [String] -> TypedExp -> Decl
makeDecl t n []   x = Decl t n x
makeDecl t n args x = makeDecl t n (init args) (AbsInf (last args) x)

parseError :: [Token] -> a
parseError token = error $ "Parse error on " ++ (show token)
}
