module CodeGen where

import Syntax

import Data.List (intersperse)
--import qualified Data.Set as S
import qualified Data.Map as M

codeGenProg :: Prog -> String
codeGenProg (Prog pgMap) = concat $ intersperse "\n" $
    [ "#include <stdio.h>"
    , "#include <stdlib.h>"
    , "#include \"langdefs.h\""
    , "#include \"compiled.h\""
    , ""
    , "int main() {"
    , "    Exp *template = instantiate_main();"
    , "    Exp *temporary = NULL;"
    , "    bool normalForm = false;"
    , "    while(!normalForm) {"
    , "        normalForm = true;"
    , "        template = reduceTemplate(&normalForm, &template);"
    , "    }"
    , ""
    , "    // Loop ended, so reduction complete. Print the remaining expression"
    , "    // and exit."
    , "    printExp(template);"
    , "    return 0;"
    , "}"
    , ""
    , "Exp *instantiate(char *funcName) {"
    ] ++ codeGenIndexFunc fList ++
    [ "    else {"
    , "        printf(\"ERROR: instantiate(): function \'%s\' not found\\n\","
    , "            funcName);"
    , "        exit(EXIT_FAILURE);"
    , "    }"
    , "}"
    ] ++ codeGenFuncs fList
    where fList = map snd $ M.toList pgMap

codeGenIndexFunc :: [Func] -> [String]
codeGenIndexFunc fs = concat $ intersperse ["    else"] $
    map codeGenIndexEntry fs

codeGenIndexEntry :: Func -> [String]
codeGenIndexEntry f =
    [ "if (strEqual(funcName, \"" ++ getName f ++ "\")) {"
    , "    return instantiate_" ++ getName f ++ "();"
    , "}"
    ]

-- Generate code for multiple functions
codeGenFuncs :: [Func] -> [String]
codeGenFuncs fs = concat $ map codeGenFunc fs

codeGenFunc :: Func -> [String]
codeGenFunc f =
    [ ""
    , "Exp *instantiate_" ++ getName f ++ "() {"
    , "    return"
    ] ++ codeGenTerm 2 (toLambdas f) ++
    [ "    ;"
    , "}"
    ]

codeGenTerm :: Int -> Term -> [String]
codeGenTerm tabNo exp = let t = tabs "" tabNo in case exp of
    App m n ->
        [ t ++ "newApp(" ]
        ++ codeGenTerm (tabNo + 1) m ++
        [ t ++ "," ]
        ++ codeGenTerm (tabNo + 1) n ++
        [ t ++ ")" ]
    AbsInf v m ->
        [ t ++ "newAbs(\"" ++ v ++ "\", " ]
        ++ codeGenTerm (tabNo + 1) m ++
        [ t ++ ")" ]
    Abs    v _ m -> codeGenTerm tabNo (AbsInf v m)
    Var    v     -> ["newVar(\"" ++ v ++ "\")"]
    Constant  c  -> ["newCon(" ++ codeGenVal c ++ ")"]
    Operation ot -> ["newOpn(" ++ codeGenOp ot ++ ")"]

codeGenVal :: Value -> String
codeGenVal val = case val of
    BoolVal False -> "0"
    BoolVal True  -> "1"
    IntVal  i     -> show i
    CharVal c     -> "(int) " ++ show c

codeGenOp :: OpType -> String
codeGenOp ot = case ot of
    { Add   -> "O_Add"   ; Sub   -> "O_Sub"   ; Mul  -> "O_Mul"
    ; Div   -> "O_Div"   ; Mod   -> "O_Mod"   ; Lss  -> "O_Lss"
    ; LsE   -> "O_LsE"   ; Equ   -> "O_Equ"   ; NEq  -> "O_NEq"
    ; Gtr   -> "O_Gtr"   ; GtE   -> "O_GtE"   ; And  -> "O_And"
    ; Or    -> "O_Or"    ; Xor   -> "O_Xor"   ; Not  -> "O_Not"
    ; IsZ   -> "O_IsZ"   ; Empty -> "O_Empty" ; Cons -> "O_Cons"
    ; Null  -> "O_Null"  ; Head  -> "O_Head"  ; Tail -> "O_Tail"
    ; Fix   -> "O_Fix"   ; Cond  -> "O_Cond"  ; InjL -> "O_InjL"
    ; InjR  -> "O_InjR"  ; RemL  -> "O_RemL"  ; RemR -> "O_RemR"
    ; Tuple -> "O_Tuple" ; Fst   -> "O_Fst"   ; Snd  -> "O_Snd"
    }

-- Generate the given number of tabs (using spaces, because hard tabs are
-- evil). The String parameter is an accumulator, call with 'tabs "" myInt'
tabs :: String -> Int -> String
tabs acc 0         = acc
tabs acc n | n > 0 = tabs (' ' : ' ' : ' ' : ' ' : acc) (n - 1)
           | otherwise = error "tabs: negative argument"
