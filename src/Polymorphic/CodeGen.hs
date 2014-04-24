module CodeGen where

{-
 -  Assemble generated code on x86_64 systems with the command:
 -      gcc -m32 asmfile.s -o asmfile
 -  and on x86 systems with:
 -      gcc asmfile.s -o asmfile
 -}

import Syntax

import Data.List (intersperse)
import qualified Data.Set as S
import qualified Data.Map as M

codeGenProg :: Prog -> String
codeGenProg pg = case pg of
    Prog pgMap ->
        concat $ intersperse "\n" $
            [".globl main"]     ++
            ["main:"]           ++
            ["call __main__"] ++
            ["movl %eax, %ebx"] ++      -- OS expects program result in %ebx
            ["movl $1, %eax"]   ++      -- System call type 1 is an exit call
            ["int $0x80"]       ++ (    -- Do the system call
            concat $ map snd $ map (codeGenFunc 0) $ map snd (M.toList pgMap))

-- Most of the work for function calls is handled on the caller's side - we need
-- not do anything more than include an address to jump to, and a return
-- instruction.
codeGenFunc :: Int -> Func -> (Int, [String])
codeGenFunc nextLabel f =
    let (nextNextLabel, codeF) = codeGenTerm nextLabel (getBody f)
    in
    (nextNextLabel,
        ["__" ++ getName f ++ "__:"]
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
            ++ ["cmp $0, %ax"]
            ++ ["je condFalse" ++ show nextNextLabel]
            ++ codeM
            ++ ["jmp condEnd" ++ show nextNextLabel]
            ++ ["condFalse" ++ show nextNextLabel ++ ":"]
            ++ codeN
            ++ ["condEnd" ++ show nextNextLabel ++ ":"]
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
        , "mov $1, %ax"     -- Move a 1 to %ax.
        , "cmp %ax, %bx"    -- Compare our value in %bx, with the 1 in %ax.
        , "cmove $0, %ax"   -- If our value is 1, move a 0 into %ax. If it's 0,
        ]                   -- leave the 1 there.

    Lss -> boolComparison "cmovl $1, %ax"
    LsE -> boolComparison "cmovle $1, %ax"
    NEq -> boolComparison "cmovne $1, %ax"
    Gtr -> boolComparison "cmovg $1, %ax"
    GtE -> boolComparison "cmovge $1, %ax"

    _   -> error $ "\'" ++ show ot ++ "\' not yet implemented"

-- Create code to do a boolean comparison
boolComparison :: String -> [String]
boolComparison comp =
    [ "movl %eax, %ecx"
    , "mov $0, %ax"
    , "cmpl %ecx, %ebx"
    , comp
    ]

lambdaLiftProg :: Prog -> Prog
lambdaLiftProg pg = pg

lambdaLiftFunc :: Func -> [Func]
lambdaLiftFunc f =
    let (newBody, newFuncs) = lambdaLiftTerm (getBody f) [] in
        case f of
            Func ty nm args _ -> Func ty nm args newBody : newFuncs
            FuncInf nm args _ -> FuncInf nm args newBody : newFuncs

lambdaLiftTerm :: Term -> (Term, [Func])
lambdaLiftTerm tm = case tm of
    Abs v t m   -> (Var nm, Func ty nm (S.toList (freeVars tm)) liftedM : funcsM)
      where
        nm =
        ty =
        (liftedM, funcsM) = lambdaLiftTerm m
    AbsInf v m  -> error "lambdaLiftTerm not yet implemented"
      where
        (liftedM, funcsM) = lambdaLiftTerm m
    Var _       -> (tm, [])
    App m n     -> (App liftedM liftedN, funcsM ++ funcsN)
      where
        (liftedM, funcsM) = lambdaLiftTerm m
        (liftedN, funcsN) = lambdaLiftTerm n
    Constant  _ -> (tm, [])
    Operation _ -> (tm, [])

makeCall :: Name -> [Name] -> Term
makeCall nm (arg:args) = App
