module Main where

import Lexer
import qualified Parser as P
import Syntax
import Unifier
import Interpreter

import System.Environment (getArgs)

-- Convert the command line arguments from a haskell list of strings into an
-- equivalent Term for use by the interpreted program
parseArgs :: [String] -> Term
parseArgs []         = Operation Empty
parseArgs (arg:args) = 
    App (App (Operation Cons) (P.parseStr arg)) (parseArgs args)

main = do
    args <- getArgs
    
    if (length args) == 0 then do
        putStrLn "Please provide a filename"

    else do
        progStr <- readFile (head args)
        let tokens     = scan progStr
            argsToProg = Func (TList (TList TChar))
                           "args" [] (parseArgs (tail args))
            progPTVars = addFunc argsToProg (P.parse tokens)
            prog       = convertAllTVars progPTVars -- Convert string TVars into integer TVars
            unifyRes   = infer (allToLambdas prog)
            hasMain    = hasFunc prog "main"
            in
                putStrLn $
                    if unifyRes == Nothing then
                        "Type check failure"
                    else if not hasMain then
                        "No main function found"
                    else if numArgs (getFunc prog "main") /= 0 then
                        "main function must take exactly 0 arguments"
                    else
                        show (reduceNorm prog (Var "main"))
                        --show (reduceNorm nilProg (allToLambdas prog))
