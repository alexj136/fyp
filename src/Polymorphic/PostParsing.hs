module PostParsing where

-- The PostParsing module contains functions and data definitions that are
-- required after the parsing phase, prior to the type check.

import Syntax
import qualified Data.Set as S
import qualified Data.Map as M

type TyDec = (Name, Type)

-- The ParseResult type is the return type for the parser - it contains a list
-- of functions, a list of type declarations
data ParseResult = ParseResult (M.Map Name Func) [TyDec]

instance Show ParseResult where
    show (ParseResult pgMap ((nm, ty):tyDecs)) =
        ("type " ++ nm ++ " = " ++ show ty ++ "\n" ++ 
        show (ParseResult pgMap tyDecs))
    show (ParseResult pgMap []) = show (Prog pgMap)

-- An empty ParseResult used as the 'base case' for recursion in the parser
emptyPR :: ParseResult
emptyPR = ParseResult M.empty []

-- Add a new function to a ParseResult
addFuncPR :: ParseResult -> Func -> ParseResult
addFuncPR (ParseResult pgMap ts) f = case M.lookup (getName f) pgMap of
    Nothing -> ParseResult (M.insert (getName f) f pgMap) ts
    Just _  -> error ("Multiple definitions of function '" ++ getName f ++ "'")

-- Add a new type declaration to a ParseResult
addTyDec :: ParseResult -> TyDec -> ParseResult
addTyDec (ParseResult pgMap ts) t = ParseResult pgMap (t:ts)

-- Combine all TyDec objects in the ParseResult with their accompanying function
-- definitions, returning a Prog object ready for the latter stages of the pipeline.
combineTyDecs:: ParseResult -> Prog
combineTyDecs (ParseResult pgMap [])                = Prog pgMap
combineTyDecs (ParseResult pgMap ((nm, ty):tyDecs)) =
    case M.lookup nm pgMap of
        Nothing ->
            error ("Type declaration '" ++ nm ++ "' has no matching function")
        Just f  ->
            combineTyDecs (ParseResult
                (M.insert nm (addTypeLabel f ty) pgMap) tyDecs
            )
