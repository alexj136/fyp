module Compiler where

import Syntax

codeGenProg :: Prog -> [String]
codeGenProg _ = error "codeGenProg not yet implemented"

-- Most of the work for function calls is handled on the caller's side - we need
-- not do anything more than include an address to jump to, and a return
-- instruction.
codeGenFunc :: Func -> [String]
codeGenFunc f =
    [getName f ++ "_entry:"]
    ++ codeGenTerm (getBody f)
    ++ ["ret"]

codeGenTerm :: Term -> [String]
codeGenTerm exp = case exp of

    -- Constants
    Constant (IntVal  x    ) -> ["movl $" ++ show x ++ ", %eax"]
    Constant (BoolVal True ) -> ["movl $1, %eax"]
    Constant (BoolVal False) -> ["movl $0, %eax"]
    Constant (CharVal c    ) -> ["mov '" ++ c : ", %ax"]

    -- Binary operations
    App (App (Operation ot) m) n | isBinary ot ->
        (codeGenTerm n)
        ++ ["pushl %eax"]
        ++ (codeGenTerm m)
        ++ ["popl %ebx"]
        ++ (codeGenOp ot)

    -- Unary operations
    App (Operation ot) m | isUnary ot ->
        (codeGenTerm m)
        ++ (codeGenOp ot)

    -- Abstractions should be lifted into functions before compilation
    Abs  _ _ _ -> error "Unlifted abstraction"
    AbsInf _ _ -> error "Unlifted abstraction"

codeGenOp :: OpType -> [String]
codeGenOp ot = case ot of
    Add -> ["addl %ebx, %eax"]
    Sub -> ["subl %ebx, %eax"]
    Mul -> ["imull %ebx, %eax"]
    Div -> ["idivl %ebx, %eax"]
    Mod -> ["idivl %ebx, %eax", "movl %edx, %eax"] -- idivl puts remndr in %edx

    And -> ["andl %ebx, %eax"]
    Or  -> ["orl %ebx, %eax"]
    Xor -> ["xorl %ebx, %eax"]

    Not -> ["movl $1, %ebx", "cmove ..."]

lambdaLiftProg :: Prog -> Prog
lambdaLiftProg _ = error "lambdaLiftProg not yet implemented"

lambdaLiftFunc :: Func -> Func
lambdaLiftFunc _ = error "lambdaLiftFunc not yet implemented"
