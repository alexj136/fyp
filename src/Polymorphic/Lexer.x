{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$lower = a-z
$upper = A-Z
$alpha = [$lower $upper]
$alnum = [$alpha $digit]

tokens :-
    $white+                ;
    \#.*\n                 ; -- Single line comments with '#'
    \#\#(.|\n)*\#\#        ; -- Multi-line comments with '##'

    \(                     { \p s -> TokenOpenBr (pos p)                 }
    \)                     { \p s -> TokenClosBr (pos p)                 }
    \[                     { \p s -> TokenOpenSq (pos p)                 }
    \]                     { \p s -> TokenClosSq (pos p)                 }
    \{                     { \p s -> TokenOpenCr (pos p)                 }
    \}                     { \p s -> TokenClosCr (pos p)                 }
    "="                    { \p s -> TokenEquals (pos p)                 }
    \,                     { \p s -> TokenComma  (pos p)                 }
    ":"                    { \p s -> TokenColon  (pos p)                 }
    "_"                    { \p s -> TokenUndrSc (pos p)                 }

    "alias"                { \p s -> TokenAlias  (pos p)                 }
    "def"                  { \p s -> TokenDef    (pos p)                 }
    "type"                 { \p s -> TokenType   (pos p)                 }

    "^"                    { \p s -> TokenLambda (pos p)                 }
    "λ"                    { \p s -> TokenLambda (pos p)                 }
    \\                     { \p s -> TokenLambda (pos p)                 }
    \.                     { \p s -> TokenDot    (pos p)                 }

    "+"                    { \p s -> TokenAdd    (pos p)                 }
    "-"                    { \p s -> TokenSub    (pos p)                 }
    "*"                    { \p s -> TokenMul    (pos p)                 }
    "/"                    { \p s -> TokenDiv    (pos p)                 }
    "%"                    { \p s -> TokenMod    (pos p)                 }

    "=="                   { \p s -> TokenEqEq   (pos p)                 }
    "<="                   { \p s -> TokenLsEq   (pos p)                 }
    "<"                    { \p s -> TokenLess   (pos p)                 }
    ">="                   { \p s -> TokenGtEq   (pos p)                 }
    ">"                    { \p s -> TokenGrtr   (pos p)                 }
    "/="                   { \p s -> TokenNoEq   (pos p)                 }
    
    "and"                  { \p s -> TokenAnd    (pos p)                 }
    "or"                   { \p s -> TokenOr     (pos p)                 }
    "xor"                  { \p s -> TokenXor    (pos p)                 }
    "not"                  { \p s -> TokenNot    (pos p)                 }
    "iszero"               { \p s -> TokenIsZero (pos p)                 }

    "let"                  { \p s -> TokenLet    (pos p)                 }
    "in"                   { \p s -> TokenIn     (pos p)                 }

    "if"                   { \p s -> TokenIf     (pos p)                 }
    "then"                 { \p s -> TokenThen   (pos p)                 }
    "else"                 { \p s -> TokenElse   (pos p)                 }

    "reml"                 { \p s -> TokenRemL   (pos p)                 }
    "remr"                 { \p s -> TokenRemR   (pos p)                 }
    "snd"                  { \p s -> TokenFst    (pos p)                 }
    "fst"                  { \p s -> TokenSnd    (pos p)                 }
    "head"                 { \p s -> TokenHead   (pos p)                 }
    "tail"                 { \p s -> TokenTail   (pos p)                 }
    "null"                 { \p s -> TokenNull   (pos p)                 }

    "Int"                  { \p s -> TokenTyInt  (pos p)                 }
    "Bool"                 { \p s -> TokenTyBool (pos p)                 }
    "Char"                 { \p s -> TokenTyChar (pos p)                 }
    "String"               { \p s -> TokenTyStr  (pos p)                 }
    "|"                    { \p s -> TokenTySum  (pos p)                 }
    "&"                    { \p s -> TokenTyProd (pos p)                 }
    "->"                   { \p s -> TokenTyArrw (pos p)                 }

    0|[1-9][0-9]*          { \p s -> TokenInt    (pos p) (read s)        }
    "true"                 { \p s -> TokenBool   (pos p) True            }
    "false"                { \p s -> TokenBool   (pos p) False           }
    \".*\"                 { \p s -> TokenStr    (pos p) (init (tail s)) }

    $lower [$alnum \_ \']* { \p s -> TokenIdLC   (pos p) s               }
    $upper [$alnum \_ \']* { \p s -> TokenIdUC   (pos p) s               }

{
data Token
    -- BASIC TOKENS
    = TokenOpenBr (Int, Int)    -- Open-bracket
    | TokenClosBr (Int, Int)    -- Close-bracket
    | TokenOpenSq (Int, Int)    -- Open square bracket
    | TokenClosSq (Int, Int)    -- Close square bracket
    | TokenOpenCr (Int, Int)    -- Open curly bracket
    | TokenClosCr (Int, Int)    -- Close curly bracket
    | TokenEquals (Int, Int)    -- Delimits declarations from function bodies
    | TokenComma  (Int, Int)    -- Commas - list delimiters
    | TokenColon  (Int, Int)    -- Colons to separate function names from their
                                -- types
    | TokenUndrSc (Int, Int)    -- Underscore '_'

    -- PROGRAM STRUCTURE
    | TokenAlias  (Int, Int)    -- Type aliases
    | TokenDef    (Int, Int)    -- Function definitions
    | TokenType   (Int, Int)    -- Function Type declarations

    -- LAMBDA SYNTAX
    | TokenLambda (Int, Int)    -- Lambda abstractions
    | TokenDot    (Int, Int)    -- Delimits abstractions from funtion bodies

    -- OPERATORS
    | TokenAdd    (Int, Int)    -- Addition operator
    | TokenSub    (Int, Int)    -- Subtraction operator
    | TokenMul    (Int, Int)    -- Multiplucation operator
    | TokenDiv    (Int, Int)    -- Integer division operator
    | TokenMod    (Int, Int)    -- Modulo operator

    | TokenEqEq   (Int, Int)    -- '=='
    | TokenLsEq   (Int, Int)    -- '<='
    | TokenLess   (Int, Int)    -- '<'
    | TokenGtEq   (Int, Int)    -- '>='
    | TokenGrtr   (Int, Int)    -- '>'
    | TokenNoEq   (Int, Int)    -- '/='

    | TokenAnd    (Int, Int)    -- Boolean and
    | TokenOr     (Int, Int)    -- Boolean or
    | TokenNot    (Int, Int)    -- Boolean not
    | TokenXor    (Int, Int)    -- Boolean xor
    | TokenIsZero (Int, Int)    -- 'Is-zero' function token

    -- LET-IN BINDINGS
    | TokenLet    (Int, Int)
    | TokenIn     (Int, Int)

    -- CONDITIONALS
    | TokenIf     (Int, Int)
    | TokenThen   (Int, Int)
    | TokenElse   (Int, Int)

    -- LIST/TUPLE/SUM TOKENS
    | TokenRemL   (Int, Int)    -- Sum remove-left
    | TokenRemR   (Int, Int)    -- Sum remove-right
    | TokenFst    (Int, Int)    -- Product first
    | TokenSnd    (Int, Int)    -- Product second
    | TokenHead   (Int, Int)    -- List head function
    | TokenTail   (Int, Int)    -- List tail function
    | TokenNull   (Int, Int)    -- List null 'is-empty' function

    -- TYPE TOKENS
    | TokenTyInt  (Int, Int)
    | TokenTyBool (Int, Int)
    | TokenTyChar (Int, Int)
    | TokenTyStr  (Int, Int)
    | TokenTySum  (Int, Int)    -- '|'
    | TokenTyProd (Int, Int)    -- '&'
    | TokenTyArrw (Int, Int)    -- '->'

    -- LITERALS
    | TokenInt    (Int, Int) Int
    | TokenBool   (Int, Int) Bool
    | TokenStr    (Int, Int) String

    -- IDENTIFIERS
    | TokenIdLC   (Int, Int) String  -- Lower-case lead identifier (carries
                                     -- identifier text)
    | TokenIdUC   (Int, Int) String  -- Upper-case lead identifier (carries
                                     -- identifier text)
    deriving (Show, Eq)

getY :: Token -> Int
getY t = case t of
    TokenOpenBr (y, _)   -> y
    TokenClosBr (y, _)   -> y
    TokenOpenSq (y, _)   -> y
    TokenClosSq (y, _)   -> y
    TokenOpenCr (y, _)   -> y
    TokenClosCr (y, _)   -> y
    TokenEquals (y, _)   -> y
    TokenComma  (y, _)   -> y
    TokenColon  (y, _)   -> y
    TokenUndrSc (y, _)   -> y

    TokenLambda (y, _)   -> y
    TokenDot    (y, _)   -> y

    TokenAdd    (y, _)   -> y
    TokenSub    (y, _)   -> y
    TokenMul    (y, _)   -> y
    TokenDiv    (y, _)   -> y
    TokenMod    (y, _)   -> y

    TokenEqEq   (y, _)   -> y
    TokenLsEq   (y, _)   -> y
    TokenLess   (y, _)   -> y
    TokenGtEq   (y, _)   -> y
    TokenGrtr   (y, _)   -> y
    TokenNoEq   (y, _)   -> y

    TokenAnd    (y, _)   -> y
    TokenOr     (y, _)   -> y
    TokenNot    (y, _)   -> y
    TokenXor    (y, _)   -> y
    TokenIsZero (y, _)   -> y

    TokenLet    (y, _)   -> y
    TokenIn     (y, _)   -> y

    TokenIf     (y, _)   -> y
    TokenThen   (y, _)   -> y
    TokenElse   (y, _)   -> y

    TokenRemL   (y, _)   -> y
    TokenRemR   (y, _)   -> y
    TokenFst    (y, _)   -> y
    TokenSnd    (y, _)   -> y
    TokenHead   (y, _)   -> y
    TokenTail   (y, _)   -> y
    TokenNull   (y, _)   -> y

    TokenTyInt  (y, _)   -> y
    TokenTyBool (y, _)   -> y
    TokenTyChar (y, _)   -> y
    TokenTyStr  (y, _)   -> y
    TokenTySum  (y, _)   -> y
    TokenTyProd (y, _)   -> y
    TokenTyArrw (y, _)   -> y

    TokenInt    (y, _) _ -> y
    TokenBool   (y, _) _ -> y
    TokenStr    (y, _) _ -> y

    TokenIdLC   (y, _) _ -> y
    TokenIdUC   (y, _) _ -> y

getX :: Token -> Int
getX t = case t of
    TokenOpenBr (_, x)   -> x
    TokenClosBr (_, x)   -> x
    TokenOpenSq (_, x)   -> x
    TokenClosSq (_, x)   -> x
    TokenOpenCr (_, x)   -> x
    TokenClosCr (_, x)   -> x
    TokenEquals (_, x)   -> x
    TokenComma  (_, x)   -> x
    TokenColon  (_, x)   -> x
    TokenUndrSc (_, x)   -> x

    TokenLambda (_, x)   -> x
    TokenDot    (_, x)   -> x

    TokenAdd    (_, x)   -> x
    TokenSub    (_, x)   -> x
    TokenMul    (_, x)   -> x
    TokenDiv    (_, x)   -> x
    TokenMod    (_, x)   -> x

    TokenEqEq   (_, x)   -> x
    TokenLsEq   (_, x)   -> x
    TokenLess   (_, x)   -> x
    TokenGtEq   (_, x)   -> x
    TokenGrtr   (_, x)   -> x
    TokenNoEq   (_, x)   -> x

    TokenAnd    (_, x)   -> x
    TokenOr     (_, x)   -> x
    TokenNot    (_, x)   -> x
    TokenXor    (_, x)   -> x
    TokenIsZero (_, x)   -> x

    TokenLet    (_, x)   -> x
    TokenIn     (_, x)   -> x

    TokenIf     (_, x)   -> x
    TokenThen   (_, x)   -> x
    TokenElse   (_, x)   -> x

    TokenRemL   (_, x)   -> x
    TokenRemR   (_, x)   -> x
    TokenFst    (_, x)   -> x
    TokenSnd    (_, x)   -> x
    TokenHead   (_, x)   -> x
    TokenTail   (_, x)   -> x
    TokenNull   (_, x)   -> x

    TokenTyInt  (_, x)   -> x
    TokenTyBool (_, x)   -> x
    TokenTyChar (_, x)   -> x
    TokenTyStr  (_, x)   -> x
    TokenTySum  (_, x)   -> x
    TokenTyProd (_, x)   -> x
    TokenTyArrw (_, x)   -> x

    TokenInt    (_, x) _ -> x
    TokenBool   (_, x) _ -> x
    TokenStr    (_, x) _ -> x

    TokenIdLC   (_, x) _ -> x
    TokenIdUC   (_, x) _ -> x

-- The lexer function
scan :: String -> [Token]
scan = alexScanTokens

-- Extract token coordinates from AlexPosn object
pos :: AlexPosn -> (Int, Int)
pos (AlexPn i j k) = (j, k)
}
