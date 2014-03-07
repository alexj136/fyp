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
    , "    int data;"
    , "};"

    -- Create a new empty stack
    , "Stack *newStack() {"
    , "    Stack *stk = safeAlloc(sizeof(Stack));"
    , "    stk -> top = NULL;"
    , "    return stk;"
    , "}"

    -- Create a new stack frame with some given data
    , "StackFrame *newStackFrame(StackFrame *behindNew, int data) {"
    , "    StackFrame *stkFm = safeAlloc(sizeof(StackFrame));"
    , "    stkFm -> data = data;"
    , "    return stkFm;"
    , "}"

    , "int main() {"
    , "    Stack *compStack = newStack();"
    , "    Stack *valStack  = newStack();"
    ,      code
    , "}"
    ] 

compileExp :: Term -> String
compileExp exp = case exp of
    Constant (IntVal  x    ) -> "regAcc = " ++ show x ++ ";"
    Constant (BoolVal True ) -> "regAcc = 1;"
    Constant (BoolVal False) -> "regAcc = 0;"
    Constant (CharVal x    ) -> "regAcc = " ++ show x ++ ";"
