{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$lower = a-z
$upper = A-Z
$alpha = [$lower $upper]
$alnum = [$alpha $digit]

tokens :-
    $white+                ;
    \#*\n                  ; -- Single line comments with '#'
    \#\#*\#\#              ; -- Multi-line comments with '##'

    \(                     { \s -> TokenOpenBr              }
    \)                     { \s -> TokenClosBr              }
    \[                     { \s -> TokenOpenSq              }
    \]                     { \s -> TokenClosSq              }
    \{                     { \s -> TokenOpenCr              }
    \}                     { \s -> TokenClosCr              }
    "="                    { \s -> TokenEquals              }
    \,                     { \s -> TokenComma               }
    ":"                    { \s -> TokenColon               }
    "_"                    { \s -> TokenUndrSc              }

    "^"                    { \s -> TokenLambda              }
    "λ"                    { \s -> TokenLambda              }
    \\                     { \s -> TokenLambda              }
    \.                     { \s -> TokenDot                 }

    "+"                    { \s -> TokenAdd                 }
    "-"                    { \s -> TokenSub                 }
    "*"                    { \s -> TokenMul                 }
    "/"                    { \s -> TokenDiv                 }
    "%"                    { \s -> TokenMod                 }

    "=="                   { \s -> TokenEqEq                }
    "<="                   { \s -> TokenLsEq                }
    "<"                    { \s -> TokenLess                }
    ">="                   { \s -> TokenGtEq                }
    ">"                    { \s -> TokenGrtr                }
    "/="                   { \s -> TokenNoEq                }
    
    "and"                  { \s -> TokenAnd                 }
    "or"                   { \s -> TokenOr                  }
    "xor"                  { \s -> TokenXor                 }
    "not"                  { \s -> TokenNot                 }
    "iszero"               { \s -> TokenIsZero              }

    "let"                  { \s -> TokenLet                 }
    "in"                   { \s -> TokenIn                  }

    "if"                   { \s -> TokenIf                  }
    "then"                 { \s -> TokenThen                }
    "else"                 { \s -> TokenElse                }

    "Int"                  { \s -> TokenTyInt               }
    "Bool"                 { \s -> TokenTyBool              }
    "Char"                 { \s -> TokenTyChar              }
    "String"               { \s -> TokenTyStr               }
    "|"                    { \s -> TokenTySum               }
    "&"                    { \s -> TokenTyProd              }
    "->"                   { \s -> TokenTyArrw              }

    [1-9][0-9]*            { \s -> TokenInt (read s)        }
    "true"                 { \s -> TokenBool True           }
    "false"                { \s -> TokenBool False          }
    \"*\"                  { \s -> TokenStr (init (tail s)) }

    $lower [$alnum \_ \']* { \s -> TokenIdLC s              }
    $upper [$alnum \_ \']* { \s -> TokenIdUC s              }

{
data Token
    -- BASIC TOKENS
    = TokenOpenBr       -- Open-bracket
    | TokenClosBr       -- Close-bracket
    | TokenOpenSq       -- Open square bracket
    | TokenClosSq       -- Close square bracket
    | TokenOpenCr       -- Open curly bracket
    | TokenClosCr       -- Close curly bracket
    | TokenEquals       -- Delimits declarations from function bodies
    | TokenComma        -- Commas - list delimiters
    | TokenColon        -- Colons to separate function names from their types
    | TokenUndrSc       -- Underscore '_'

    -- LAMBDA SYNTAX
    | TokenLambda       -- Lambda abstractions
    | TokenDot          -- Delimits abstractions from funtion bodies

    -- OPERATORS
    | TokenAdd          -- Addition operator
    | TokenSub          -- Subtraction operator
    | TokenMul          -- Multiplucation operator
    | TokenDiv          -- Integer division operator
    | TokenMod          -- Modulo operator

    | TokenEqEq         -- '=='
    | TokenLsEq         -- '<='
    | TokenLess         -- '<'
    | TokenGtEq         -- '>='
    | TokenGrtr         -- '>'
    | TokenNoEq         -- '/='

    | TokenAnd          -- Boolean and
    | TokenOr           -- Boolean or
    | TokenNot          -- Boolean not
    | TokenXor          -- Boolean xor
    | TokenIsZero       -- 'Is-zero' function token

    -- LET-IN BINDINGS
    | TokenLet
    | TokenIn

    -- CONDITIONALS
    | TokenIf
    | TokenThen
    | TokenElse

    -- TYPE TOKENS
    | TokenTyInt
    | TokenTyBool
    | TokenTyChar
    | TokenTyStr
    | TokenTySum        -- '|'
    | TokenTyProd       -- '&'
    | TokenTyArrw       -- '->'

    -- LITERALS
    | TokenInt  Int
    | TokenBool Bool
    | TokenStr  String

    -- IDENTIFIERS
    | TokenIdLC String  -- Lower-case lead identifier (carries identifier text)
    | TokenIdUC String  -- Upper-case lead identifier (carries identifier text)

    deriving (Show, Eq)

-- The lexer function
scan :: String -> [Token]
scan = alexScanTokens
}
