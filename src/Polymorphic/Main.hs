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
        let tokens      = scan progStr
            argsToProg  = Func (TList (TList TChar))
                            "args" [] (parseArgs (tail args))
            prog        = addFunc argsToProg (P.parse tokens)
            unifyRes    = infer prog
            hasMain     = hasFunc prog "main"
            in
                putStrLn $
                    if unifyRes == Nothing then
                        "Type check failure"
                    else if not hasMain then
                        "No main function found"
                    else
                        show (reduce prog (Var "main"))
