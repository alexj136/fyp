module Main where

import Lexer
import qualified Parser as P
import PostParsing
import Syntax
import Unifier
import Interpreter
import CodeGen

import System.Environment (getArgs)
import System.Cmd (system)
import qualified Data.Map as M

-- Convert the command line arguments from a haskell list of strings into an
-- equivalent Term for use by the interpreted program
parseArgs :: [String] -> Term
parseArgs []         = Operation Empty
parseArgs (arg:args) = 
    App (App (Operation Cons) (P.parseStr arg)) (parseArgs args)

main = do
    args <- getArgs
    
    if (length args) < 2 then do
        putStrLn "usage = Main [[-c|--compile] filename |\
            \ [-i|--interpret] filename args]"

    else if (head args) == "-c" || (head args) == "--compile" then do
        progStr <- readFile $ head (tail args)
        prelude <- readFile "prelude"
        let tokens       = scan progStr ++ scan prelude
            parseRes     = P.parse tokens
            prog         = combineTyDecs parseRes
            declaresArgs = hasFunc prog "args"
            progCtx      = (snd (contextProg 0 prog))
            initialCtx   = M.insert "args" (TList (TList TChar)) progCtx
            unifyRes     = inferFull initialCtx (allToLambdas prog)
            hasMain      = hasFunc prog "main"
            outputCode   = codeGenProg prog
            in
            if declaresArgs then
                putStrLn "Program may not declare function \'args\'"
            else if unifyRes == Nothing then
                putStrLn "Type check failure"
            else if not hasMain then
                putStrLn "No main function found"
            else
                writeFile "compiled.c" outputCode >>= \_ ->
                system ("gcc -Wall -g langdefs.c compiled.c -o "
                    ++ (head (tail args)) ++ "_binary") >>= \_ ->
                return ()

    else if (head args) == "-i" || (head args) == "--interpret" then do
        progStr <- readFile (head (tail args))
        prelude <- readFile "prelude"
        let
            -- Tokenise the program, add the prelude code
            tokens = scan progStr ++ scan prelude

            -- Convert the command line arguments to hand to the program
            argsToProg = Func (TList (TList TChar))
                           "args" [] (parseArgs (tail (tail args)))

            -- Obtain a ParseResult - a map from names to functions, a list of
            -- type declarations, and a list of type aliases
            parseRes = P.parse tokens

            -- Obtain a Prog containing ParserTVars
            prog = combineTyDecs parseRes

            -- Add the command line arguments
            progWithArgs = addFunc argsToProg prog

            -- Type check the program
            unifyRes =
                inferFull
                    (snd (contextProg 0 progWithArgs))
                    (allToLambdas progWithArgs)

            -- Verify that the program has a main function
            hasMain = hasFunc progWithArgs "main"

            -- Evaluate the program and print the result
            in
                putStrLn $
                    if unifyRes == Nothing then
                        "Type check failure"
                    else if not hasMain then
                        "No main function found"
                    else if numArgs (getFunc prog "main") /= 0 then
                        "main function must take exactly 0 arguments"
                    else
                        show (reduceNorm progWithArgs (Var "main"))
    else
        putStrLn "usage = Main [[-c|--compile] filename |\
            \ [-i|--interpret] filename args]"
