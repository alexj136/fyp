{
module Parser (parse) where

import Lexer
import PolymorphicSyntax

{-- CONTEXT FREE GRAMMAR

 (GFC TO GO HERE)

--}
}

%name parse
%tokentype { Token }
%error { parseError }
%left exp

%token
    lam    { T_Lambda }
    dot    { T_Dot    }
    openbr { T_OpenBr }
    closbr { T_ClosBr }
    var    { T_Var $$ }


%%

exp :: { Exp }
exp : exp exp           { App $1 $2 } 
    | openbr exp closbr { $2 }
    | lam var dot exp   { Abs $2 $4 }
    | var               { Var (nameOf $1) }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
