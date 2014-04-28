module CodeGen where

{-
 -  Assemble generated code on x86_64 systems with the command:
 -      gcc -m32 asmfile.s -o asmfile
 -  and on x86 systems with:
 -      gcc asmfile.s -o asmfile
 -}

import Syntax
import Interpreter (replace)

import Data.List (intersperse)
import qualified Data.Set as S
import qualified Data.Map as M

codeGenProg :: Prog -> String
codeGenProg pg = case pg of
    Prog pgMap ->
        concat $ intersperse "\n" $
            [ ".globl main"
            , "main:"
            ,"call " ++ (codeName "main")
            ,"movl %eax, %ebx"  -- OS expects program result in %ebx
            ,"movl $1, %eax"    -- System call type 1 is an exit call
            ,"int $0x80"        -- Do the system call
            ] ++ (snd (codeGenFuncs 0 (map snd (M.toList pgMap))))

-- Generate code for multiple functions
codeGenFuncs :: Int -> [Func] -> (Int, [String])
codeGenFuncs = codeGenFuncsAcc []
  where
    codeGenFuncsAcc :: [String] -> Int -> [Func] -> (Int, [String])
    codeGenFuncsAcc acc nextLabel []     = (nextLabel, acc)
    codeGenFuncsAcc acc nextLabel (f:fs) =
        let (nextLabel2, fCode) = codeGenFunc nextLabel f in
            codeGenFuncsAcc (acc ++ fCode) nextLabel2 fs

-- Most of the work for function calls is handled on the caller's side - we need
-- not do anything more than include an address to jump to, and a return
-- instruction.
codeGenFunc :: Int -> Func -> (Int, [String])
codeGenFunc nextLabel f =
    let (nextNextLabel, codeF) = codeGenTerm nextLabel (getBody f)
    in
    (nextNextLabel,
        [(codeName (getName f)) ++ ":"]
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
        let (nextLabel2, codeGuard) = codeGenTerm nextLabel  guard
            (nextLabel3, codeM    ) = codeGenTerm nextLabel2 m
            (nextLabel4, codeN    ) = codeGenTerm nextLabel3 n
        in
        (nextLabel4,
            codeGuard
            ++ ["cmp $0, %ax"]
            ++ ["je condFalse" ++ show nextLabel2]
            ++ codeM
            ++ ["jmp condEnd" ++ show nextLabel2]
            ++ ["condFalse" ++ show nextLabel2 ++ ":"]
            ++ codeN
            ++ ["condEnd" ++ show nextLabel2 ++ ":"]
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
    let (_, newBody, newFuncs) = lambdaLiftTerm (getName f) 0 (getBody f) in
        case f of
            Func ty nm args _ -> Func ty nm args newBody : newFuncs
            FuncInf nm args _ -> FuncInf nm args newBody : newFuncs

-- Takes the name of the containing function, an integer representing the next
-- usable function name for functions we lift out, and the term to lift.
lambdaLiftTerm :: Name -> Int -> Term -> (Int, Term, [Func])
lambdaLiftTerm nm liftNum tm = case tm of

    App (AbsInf v m) n ->
        ( liftNum + 1
        , replace v m (makeCall v args)
        , FuncInf liftName args n : []
        )
      where
        liftName = liftCodeName nm liftNum
        args = S.toList (freeVars tm)

    -- Take a shortcut here - to lift a function with a type declaration, throw
    -- away the type declaration and lift the implicitly typed function.
    App (Abs v t m) n -> lambdaLiftTerm nm liftNum (App (AbsInf v m) n)

    Var _       -> (liftNum, tm, [])
    App m n     -> (newLiftNum2, App liftedM liftedN, funcsM ++ funcsN)
      where
        (newLiftNum , liftedM, funcsM) = lambdaLiftTerm nm    liftNum m
        (newLiftNum2, liftedN, funcsN) = lambdaLiftTerm nm newLiftNum n
    Constant  _ -> (liftNum, tm, [])
    Operation _ -> (liftNum, tm, [])

makeCall :: Name -> [Name] -> Term
makeCall nm (arg:args) = error "makeCall not yet implemented"

-- Translate a declared function name into a name that will be included as a
-- label in the generated assembly code
codeName :: Name -> Name
codeName n = "__" ++ n ++ "__"

-- Generate the name of a function that has been lifted out of an abstraction,
-- that can be used in generated assembly code
liftCodeName :: Name -> Int -> Name
liftCodeName n i = "__" ++ n ++ "__lift__" ++ show i ++ "__"
