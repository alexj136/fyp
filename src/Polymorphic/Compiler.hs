module Compiler where

import Syntax

compileTerm :: Term -> [String]
compileTerm exp = case exp of

    -- Constants
    Constant (IntVal  x    ) -> ["movl $" ++ show x ++ ", %eax"]
    Constant (BoolVal True ) -> ["movl $1, %eax"]
    Constant (BoolVal False) -> ["movl $0, %eax"]
    Constant (CharVal c    ) -> ["mov '" ++ c : ", %ax"]

    -- Binary operations
    App (App (Operation ot) m) n | isBinary ot ->
        (compileTerm n) ++
        ["pushl %eax"] ++
        (compileTerm m) ++
        ["popl %ebx"] ++
        (compileOp ot)

    -- Unary operations
    App (Operation ot) m | isUnary ot ->
        (compileTerm m) ++
        (compileOp ot)

    -- Abstractions should be lifted into functions before compilation
    Abs  _ _ _ -> error "Unlifted abstraction"
    AbsInf _ _ -> error "Unlifted abstraction"
