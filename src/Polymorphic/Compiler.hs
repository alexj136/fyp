module Compiler where

import Syntax

codeGenTerm :: Term -> [String]
codeGenTerm exp = case exp of

    -- Constants
    Constant (IntVal  x    ) -> ["movl $" ++ show x ++ ", %eax"]
    Constant (BoolVal True ) -> ["movl $1, %eax"]
    Constant (BoolVal False) -> ["movl $0, %eax"]
    Constant (CharVal c    ) -> ["mov '" ++ c : ", %ax"]

    -- Binary operations
    App (App (Operation ot) m) n | isBinary ot ->
        (codeGenTerm n) ++
        ["pushl %eax"] ++
        (codeGenTerm m) ++
        ["popl %ebx"] ++
        (codeGenOp ot)

    -- Unary operations
    App (Operation ot) m | isUnary ot ->
        (codeGenTerm m) ++
        (codeGenOp ot)

    -- Abstractions should be lifted into functions before compilation
    Abs  _ _ _ -> error "Unlifted abstraction"
    AbsInf _ _ -> error "Unlifted abstraction"

lambdaLift :: Term -> Term
lambdaLift _ = error "Lambda lifting not yet implemented"
