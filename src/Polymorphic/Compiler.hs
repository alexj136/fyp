module Compiler where

import Lexer
import qualified Parser as P
import PostParsing
import Syntax
import Unifier
import CodeGen

import System.Environment (getArgs)
import qualified Data.Map as M

main = do
    putStrLn "Warning: lambda lifting not yet implemented. Local \
        \Definitions will cause errors."
    args    <- getArgs

    if (length args) == 0 then do
        putStrLn "Please provide a filename"

    else do
        progStr <- readFile $ head args
        let tokens       = scan progStr
            parseRes     = P.parse tokens
            progPTVars   = combineTyDecs parseRes
            (i, _, prog) = convertTVarsProg (0, M.empty, progPTVars)
            unifyRes     = inferFull (snd (contextProg i prog)) (allToLambdas prog)
            hasMain      = hasFunc prog "main"
            in
            if unifyRes == Nothing then
                putStrLn "Type check failure"
            else if not hasMain then
                putStrLn "No main function found"
            else
                writeFile ((head args) ++ ".s") $ codeGenProg $ lambdaLiftProg prog
