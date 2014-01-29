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
    "^"                    { \s -> TokenLambda }
    "λ"                    { \s -> TokenLambda }
    \\                     { \s -> TokenLambda }
    \.                     { \s -> TokenDot    }
    \(                     { \s -> TokenOpenBr }
    \)                     { \s -> TokenClosBr }
    "="                    { \s -> TokenEquals }

    "+"                    { \s -> TokenAdd    }
    "-"                    { \s -> TokenSub    }
    "*"                    { \s -> TokenMul    }
    "div"                  { \s -> TokenDiv    }
    "mod"                  { \s -> TokenMod    }

    "let"                  { \s -> TokenLet    }
    "in"                   { \s -> TokenIn     }

    "not"                  { \s -> TokenNot    }
    "iszero"               { \s -> TokenIsZero }

    "True"                 { \s -> TokenBool True    }
    "False"                { \s -> TokenBool False   }

    [1-9][0-9]*            { \s -> TokenInt (read s) }
    $lower [$alnum \_ \']* { \s -> TokenIdLC s       }
    $upper [$alnum \_ \']* { \s -> TokenIdUC s       }

{
data Token =

    -- BASIC TOKENS
      TokenIdLC String  -- Lower-case lead identifier (carries identifier text)
    | TokenIdUC String  -- Upper-case lead identifier (carries identifier text)
    | TokenOpenBr       -- Open-bracket
    | TokenClosBr       -- Close-bracket
    | TokenEquals       -- Delimits declarations from function bodies

    -- LAMBDA SYNTAX
    | TokenLambda       -- Lambda abstractions
    | TokenDot          -- Delimits abstractions from funtion bodies

    -- Constants
    | TokenInt Int      -- Integer literals
    | TokenBool Bool    -- Boolean literals

    -- BINARY OPERATORS
    | TokenAdd          -- Addition operator
    | TokenSub          -- Subtraction operator
    | TokenMul          -- Multiplucation operator
    | TokenDiv          -- Integer division operator
    | TokenMod          -- Modulo operator

    -- LET-IN BINDINGS
    | TokenLet          -- let keyword
    | TokenIn           -- in keyword

    -- UNARY OPERATORS
    | TokenNot          -- Logical not
    | TokenIsZero       -- 'Is-zero' function token

    deriving (Show, Eq)

-- The lexer function
scan :: String -> [Token]
scan = alexScanTokens
}
