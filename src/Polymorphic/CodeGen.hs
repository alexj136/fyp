module CodeGen where

{-
 -  Assemble generated code on x86_64 systems with the command:
 -      gcc -m32 asmfile.s -o asmfile
 -  and on x86 systems with:
 -      gcc asmfile.s -o asmfile
 -}

import Syntax

codeGenProg :: Prog -> [String]
codeGenProg _ = error "codeGenProg not yet implemented"

-- Most of the work for function calls is handled on the caller's side - we need
-- not do anything more than include an address to jump to, and a return
-- instruction.
codeGenFunc :: Int -> Func -> (Int, [String])
codeGenFunc nextLabel f =
    let (nextNextLabel, codeF) = codeGenTerm nextLabel (getBody f)
    in
    (nextNextLabel,
        [getName f ++ "_entry:"]
        ++ codeF
        ++ ["ret"]
    )

codeGenTerm :: Int -> Term -> (Int, [String])
codeGenTerm nextLabel exp = case exp of

    -- Constants
    Constant (IntVal  x    ) -> (nextLabel, ["movl $" ++ show x ++ ", %eax"])
    Constant (BoolVal True ) -> (nextLabel, ["mov $1, %ax"])
    Constant (BoolVal False) -> (nextLabel, ["mov $0, %ax"])
    Constant (CharVal c    ) -> (nextLabel, ["mov '" ++ c : ", %ax"])

    -- Binary operations
    App (App (Operation ot) m) n | isBinary ot ->
        let (nextNextLabel    , codeN ) = codeGenTerm nextLabel     n
            (nextNextNextLabel, codeM ) = codeGenTerm nextNextLabel m
        in
        (nextNextNextLabel,
            codeN
            ++ ["pushl %eax"]
            ++ codeM
            ++ ["popl %ebx"]
            ++ codeGenOp ot
        )

    -- Unary operations
    App (Operation ot) m | isUnary ot ->
        let (nextNextLabel, codeM ) = codeGenTerm nextLabel m
        in
        (nextNextLabel,
            codeM ++
            codeGenOp ot
        )

    -- Conditionals
    App (App (App (Operation Cond) guard) m) n ->
        let (nextNextLabel        , codeGuard) = codeGenTerm nextLabel         guard
            (nextNextNextLabel    , codeM    ) = codeGenTerm nextNextLabel     m
            (nextNextNextNextLabel, codeN    ) = codeGenTerm nextNextNextLabel n
        in
        (nextNextNextNextLabel,
            codeGuard
            ++ ["cmp $1, %ax", ""]
            ++ ["labelCondTrue" ++ show nextNextLabel ++ ":"]
            ++ codeM
            ++ ["jmp labelCondEnd" ++ show nextNextLabel]
            ++ ["labelCondFalse" ++ show nextNextLabel ++ ":"]
            ++ codeN
            ++ ["labelCondEnd" ++ show nextNextLabel ++ ":"]
        )

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

    And -> ["and %ebx, %eax"]
    Or  -> ["or %ebx, %eax"]
    Xor -> ["xor %ebx, %eax"]

    Not ->
        [ "mov %ax, %bx"    -- Move the value we want to 'not' into %bx.
        , "mov $1, %ax"      -- Move a 1 to %ax.
        , "cmp %ax, %bx"    -- Compare our value in %bx, with the 1 in %ax.
        , "cmove $0, %ax"    -- If our value is 1, move a 0 into %ax. If it's 0,
        ]                   -- leave the 1 there.

lambdaLiftProg :: Prog -> Prog
lambdaLiftProg _ = error "lambdaLiftProg not yet implemented"

lambdaLiftFunc :: Func -> [Func]
lambdaLiftFunc f =
    let (newBody, newFuncs) = lambdaLiftTerm (getBody f) in
        case f of
            Func ty nm args _ -> Func ty nm args newBody : newFuncs
            FuncInf nm args _ -> FuncInf nm args newBody : newFuncs

lambdaLiftTerm :: Term -> (Term, [Func])
lambdaLiftTerm _ = error "lambdaLiftTerm not yet implemented"
