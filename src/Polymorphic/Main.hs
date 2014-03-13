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
        (putStrLn . show . P.parse . scan) progStr
        let
            -- Tokenise the program
            tokens = scan progStr

            -- Convert the command line arguments to hand to the program
            argsToProg = Func (TList (TList TChar))
                           "args" [] (parseArgs (tail args))

            -- Obtain a ParseResult - a map from names to functions, a list of
            -- type declarations, and a list of type aliases
            parseRes = P.parse tokens

            -- Obtain a Prog containing ParserTVars, and the type aliases
            (progPTVars, aliases) = combineTyDecs parseRes

            -- Convert ParserTVars into integer TVars and add the command-line
            -- arguments to to the program
            prog = addFunc argsToProg (convertTVarsProg progPTVars)

            -- Type check the program
            unifyRes = infer aliases (allToLambdas prog)

            -- Verify that the program has a main function
            hasMain = hasFunc prog "main"

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
