module CodeGen where

import Syntax

import Data.List (intersperse)
--import qualified Data.Set as S
import qualified Data.Map as M

-- Assign to each function an integer, which is negative, to be used in variable
-- nodes. These numbers are negative because ordinary variables use
-- positive De Bruijn indices.
genIndexes :: Int -> M.Map Name Func -> M.Map Name Int
genIndexes i m
    | M.null m  = M.empty
    | otherwise = let ((n, _), rest) = M.deleteFindMin m in
        M.union (M.singleton n i) (genIndexes (i + 1) rest)

codeGenProg :: Prog -> String
codeGenProg (Prog pgMap) = concat $ intersperse "\n" $
    [ "#include <stdio.h>"
    , "#include <stdlib.h>"
    , "#include \"langdefs.h\""
    ] ++ codeGenFuncs funcIndex funcList ++
    [ ""
    , "Exp *instantiate(int funcNo) {"
    ] ++ codeGenIndexFunc funcIndex funcList ++
    [ "    else {"
    , "        printf(\"ERROR: instantiate(): function \'%d\' not found\\n\","
    , "            funcNo);"
    , "        exit(EXIT_FAILURE);"
    , "    }"
    , "}"
    , ""
    , "bool hasFunc(int funcNo) {"
    ] ++ codeGenHasFunc funcIndex funcList ++
    [ "    return false;"
    , "}"
    , ""
    , "int main() {"
    , "    Exp *template = instantiate_" ++ show (funcIndex M.! "main") ++ "();"
    , "    reduceTemplateNorm(&template);"
    , "    printlnExp(template);"
    , "    freeExp(template);"
    , "    return 0;"
    , "}"
    ]
  where
    funcList  = map snd $ M.toList pgMap
    funcIndex = genIndexes 1 pgMap

codeGenHasFunc :: M.Map String Int -> [Func] -> [String]
codeGenHasFunc funcIndex fs = map (codeGenHasThisFunc funcIndex) fs

codeGenHasThisFunc :: M.Map String Int -> Func -> String
codeGenHasThisFunc funcIndex f = "    if(funcNo == -"
    ++ show (funcIndex M.! (getName f)) ++ ") return true;"

codeGenIndexFunc :: M.Map String Int -> [Func] -> [String]
codeGenIndexFunc funcIndex fs = concat $ intersperse ["    else"] $
    map (codeGenIndexEntry funcIndex) fs

codeGenIndexEntry :: M.Map String Int -> Func -> [String]
codeGenIndexEntry funcIndex f = let idStr = show (funcIndex M.! (getName f)) in
    [ "    if(funcNo == -" ++ idStr ++ ") {"
    , "        return instantiate_" ++ idStr ++ "();"
    , "    }"
    ]

-- Generate code for multiple functions
codeGenFuncs :: M.Map String Int -> [Func] -> [String]
codeGenFuncs funcIndex fs = concat $ map (codeGenFunc funcIndex) fs

codeGenFunc :: M.Map String Int -> Func -> [String]
codeGenFunc funcIndex f =
    [ ""
    , "Exp *instantiate_" ++ show (funcIndex M.! (getName f)) ++ "() {"
    , "    return"
    ] ++ codeGenTerm funcIndex M.empty 2 (toLambdas f) ++
    [ "    ;"
    , "}"
    ]

-- Generate code for the given Term. The Map parameter determines the De Bruijn
-- index that should be given to a variable. The Int parameter is used to
-- determine the number of tabs to use before a line.
codeGenTerm :: M.Map String Int -> M.Map Name Int -> Int -> Term -> [String]
codeGenTerm funcIndex bindings tabNo exp = let t = tabs "" tabNo in case exp of
    App m n ->
        [ t ++ "newApp(" ]
        ++ codeGenTerm funcIndex bindings (tabNo + 1) m ++
        [ t ++ "," ]
        ++ codeGenTerm funcIndex bindings (tabNo + 1) n ++
        [ t ++ ")" ]
    AbsInf v m ->
        [ t ++ "newAbs(" ]
        ++ codeGenTerm funcIndex (M.insert v 0 (M.map (+ 1) bindings)) (tabNo + 1) m ++
        [ t ++ ")" ]
    Abs    v _ m -> codeGenTerm funcIndex bindings tabNo (AbsInf v m)
    Var    v    | M.member v bindings ->
                    [t ++ "newVar(" ++ show (bindings M.! v) ++ ")"]
                | otherwise           ->
                    [t ++ "newVar(-" ++ show (funcIndex M.! v) ++ ")"]
    Constant  c  -> [t ++ "newCon(" ++ codeGenVal c ++ ")"]
    Operation ot -> [t ++ "newOpn(" ++ codeGenOp ot ++ ")"]

codeGenVal :: Value -> String
codeGenVal val = case val of
    BoolVal False -> "C_Bool, false"
    BoolVal True  -> "C_Bool, true"
    IntVal  i     -> "C_Int, " ++ show i
    CharVal c     -> "C_Char, (int) " ++ show c

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
