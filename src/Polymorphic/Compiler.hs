module Compiler where

import Lexer
import qualified Parser as P
import PostParsing
import Syntax
import Unifier

import System.Environment (getArgs)

main = do
    args <- getArgs
    progStr <- readFile $ head args
    let tokens = scan progStr
        parseRes = P.parse tokens
        progPTVars = combineTyDecs parseRes
