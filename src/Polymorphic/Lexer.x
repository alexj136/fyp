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
    "^"                    { \s -> TokenLambda              }
    "λ"                    { \s -> TokenLambda              }
    \\                     { \s -> TokenLambda              }
    \.                     { \s -> TokenDot                 }
    \(                     { \s -> TokenOpenBr              }
    \)                     { \s -> TokenClosBr              }
    \[                     { \s -> TokenOpenSq              }
    \]                     { \s -> TokenClosSq              }
    \{                     { \s -> TokenOpenCr              }
    \}                     { \s -> TokenClosCr              }
    "="                    { \s -> TokenEquals              }
    \,                     { \s -> TokenComma               }

    "+"                    { \s -> TokenAdd                 }
    "-"                    { \s -> TokenSub                 }
    "*"                    { \s -> TokenMul                 }
    "/"                    { \s -> TokenDiv                 }
    "%"                    { \s -> TokenMod                 }

    "<="                   { \s -> TokenLSEQ                }
    "<"                    { \s -> TokenLESS                }
    "=="                   { \s -> TokenEQEQ                }
    "/="                   { \s -> TokenNOEQ                }
    ">="                   { \s -> TokenGTEQ                }
    ">"                    { \s -> TokenGRTR                }
    
    "and"                  { \s -> TokenAnd                 }
    "or"                   { \s -> TokenOr                  }
    "xor"                  { \s -> TokenXor                 }
    "not"                  { \s -> TokenNot                 }
    "iszero"               { \s -> TokenIsZero              }
    "let"                  { \s -> TokenLet                 }
    "in"                   { \s -> TokenIn                  }
    "true"                 { \s -> TokenBool True           }
    "false"                { \s -> TokenBool False          }
    "if"                   { \s -> TokenIf                  }
    "then"                 { \s -> TokenThen                }
    "else"                 { \s -> TokenElse                }

    "Int"                  { \s -> TokenTyInt               }
    "Bool"                 { \s -> TokenTyBool              }
    "Char"                 { \s -> TokenTyChar              }
    "String"               { \s -> TokenTyStr               }
    "|"                    { \s -> TokenTySum               }
    "&"                    { \s -> TokenTyProd              }
    "->"                   { \s -> TokenTyArrow             }

    [1-9][0-9]*            { \s -> TokenInt (read s)        }
    $lower [$alnum \_ \']* { \s -> TokenIdLC s              }
    $upper [$alnum \_ \']* { \s -> TokenIdUC s              }
    \"*\"                  { \s -> TokenStr (init (tail s)) }

{
data Token
    -- BASIC TOKENS
    = TokenIdLC String  -- Lower-case lead identifier (carries identifier text)
    | TokenIdUC String  -- Upper-case lead identifier (carries identifier text)
    | TokenOpenBr       -- Open-bracket
    | TokenClosBr       -- Close-bracket
    | TokenOpenSq       -- Open square bracket
    | TokenClosSq       -- Close square bracket
    | TokenOpenCr       -- Open curly bracket
    | TokenClosCr       -- Close curly bracket
    | TokenEquals       -- Delimits declarations from function bodies
    | TokenComma        -- Commas - list delimiters
    | TokenUndrSc       -- Underscore '_'

    -- LAMBDA SYNTAX
    | TokenLambda       -- Lambda abstractions
    | TokenDot          -- Delimits abstractions from funtion bodies

    -- LITERALS
    | TokenInt  Int
    | TokenBool Bool
    | TokenStr  String

    -- OPERATORS
    | TokenAdd          -- Addition operator
    | TokenSub          -- Subtraction operator
    | TokenMul          -- Multiplucation operator
    | TokenDiv          -- Integer division operator
    | TokenMod          -- Modulo operator

    | TokenEQEQ         -- '=='
    | TokenLSEQ         -- '<='
    | TokenLESS         -- '<'
    | TokenGTEQ         -- '>='
    | TokenGRTR         -- '>'
    | TokenNOEQ         -- '/='

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
    | TokenTyArrow      -- '->'

    deriving (Show, Eq)

-- The lexer function
scan :: String -> [Token]
scan = alexScanTokens
}
