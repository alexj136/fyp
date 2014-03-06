module Compiler where

import Syntax

putHeaderFooter :: String -> String
putHeaderFooter code = concat
    [ "#include <stdio.h>"
    , "#include <malloc.h>"

    -- Registers
    , "static register int regAcc = 0; // accumulator"
    , "static register int regGP1 = 0; // general purpose 1"
    , "static register int regGP2 = 0; // general purpose 2"

    -- Stacks are used to store the state of execution
    , "typedef struct Stack Stack;"
    , "struct Stack {"
    , "    StackFrame *top;"
    , "};"

    , "typedef struct StackFrame StackFrame;"
    , "struct StackFrame {"
    , "    struct StackFrame *next;"
    , "    void *data;"
    , "};"

    , "int main() {"
    , code
    , "}"
    ] 

compileExp :: TypedExp -> String
compileExp exp = case exp of
    Constant (IntVal  x    ) -> "regAcc = " ++ show x ++ ";"
    Constant (BoolVal True ) -> "regAcc = 1;"
    Constant (BoolVal False) -> "regAcc = 0;"
    Constant (CharVal x    ) -> "regAcc = " ++ show x ++ ";"
