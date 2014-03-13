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
        let
            -- Tokenise the program
            tokens     = scan progStr

            -- Convert the command line arguments to hand to the program
            argsToProg = Func (TList (TList TChar))
                           "args" [] (parseArgs (tail args))

            -- Parse the program and include the command line arguments in the
            -- resulting program, which will contain ParserTVars
            progPTVars = addFunc argsToProg (P.parse tokens)

            -- Convert ParserTVars into integer TVars
            prog       = convertTVarsProg progPTVars

            -- Type check the program
            unifyRes   = infer (allToLambdas prog)

            -- Verify that the program has a main function
            hasMain    = hasFunc prog "main"

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
                        show (reduceNorm prog (Var "main"))
