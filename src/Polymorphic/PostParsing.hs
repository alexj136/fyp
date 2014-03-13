module PostParsing where

-- The PostParsing module contains functions and data definitions that are
-- required after the parsing phase, prior to the type check.

import Syntax
import qualified Data.Set as S
import qualified Data.Map as M

-- An alias is really the same thing as a Constraint - see the Unifier module
type Alias = (Type, Type)

type TyDec = (Name, Type)

-- The ParseResult type is the return type for the parser - it contains a list
-- of functions, a list of type declarations, and a list of type aliases.
data ParseResult = ParseResult (M.Map Name Func) [TyDec] [Alias]

-- An empty ParseResult used as the 'base case' for recursion in the parser
emptyPR :: ParseResult
emptyPR = ParseResult M.empty [] []

-- Add a new function to a ParseResult
addFuncPR :: ParseResult -> Func -> ParseResult
addFuncPR (ParseResult pgMap ts as) f = case M.lookup (getName f) pgMap of
    Nothing -> ParseResult (M.insert (getName f) f pgMap) ts as
    Just _  -> error ("Multiple definitions of function '" ++ getName f ++ "'")

-- Add a new type declaration to a ParseResult
addTyDec :: ParseResult -> TyDec -> ParseResult
addTyDec (ParseResult pgMap ts as) t = ParseResult pgMap (t:ts) as

-- Add a new type alias to a ParseResult
addAlias :: ParseResult -> Alias -> ParseResult
addAlias (ParseResult pgMap ts as) a = ParseResult pgMap ts (a:as)

-- Combine all TyDec objects in the ParseResult with their accompanying function
-- definitions, returning a Prog object and a set of aliases, ready for the
-- latter stages of the pipeline.
combineTyDecs:: ParseResult -> (Prog, S.Set Alias)
combineTyDecs (ParseResult pgMap [] as)             =
    (Prog pgMap, S.fromList as)
combineTyDecs (ParseResult pgMap (tyDec:tyDecs) as) =
    case M.lookup (fst tyDec) pgMap of
        Nothing -> error ("Type declaration '" ++ (fst tyDec) ++
                          "' has no matching function")
        Just f  -> 
            case f of
                Func _ nm _ _ -> error ("Multiple type declarations for '" ++
                                       (fst tyDec) ++ "'")
                FuncInf nm args bdy ->
                    combineTyDecs (ParseResult
                        (M.insert nm (Func (snd tyDec) nm args bdy) pgMap)
                        tyDecs
                        as
                    )

-- Convert all ParserTVars in a function into integer TVars. A fresh map object
-- is used for each function, but fresh TVar numbers are not which has the
-- effect of making the user's TVars unique to each function.
convertTVarsProg :: Prog -> Prog
convertTVarsProg (Prog pgMap) = Prog pgMap'
  where
    (_, _, pgMap') = (convertTVarsFuncMap (0, M.empty, pgMap))

    convertTVarsFuncMap ::
        (Int, M.Map String Int, M.Map Name Func) ->
        (Int, M.Map String Int, M.Map Name Func)
    convertTVarsFuncMap (i, m, pgMap)
        | M.null pgMap = (i, m, pgMap)
        | otherwise    =
            let ((_ ,f)  , fs ) = M.deleteFindMin pgMap
                (i' , m' , f' ) = convertTVarsFunc (i, m, f)
                (i'', m'', fs') = convertTVarsFuncMap (i', m', fs)
            in
                (i'', m'', M.union (M.singleton (getName f') f') fs')

-- Convert all ParserTVars in a function into integer TVars. The intege
-- parameter is the next type variable name to use, and the map stores the
-- existing equivalences between string type variables and integer type
-- variables.
convertTVarsFunc ::
    (Int, M.Map String Int, Func) -> (Int, M.Map String Int, Func)
convertTVarsFunc (i, m, f) = case f of

    Func ty nm args bdy -> (i'', m'', Func ty' nm args bdy')
        where (i' , m' , ty' ) = convertTVarsType (i , m , ty )
              (i'', m'', bdy') = convertTVarsTerm (i', m', bdy)

    FuncInf nm args bdy -> (i' , m' , FuncInf  nm args bdy')
        where (i' , m' , bdy') = convertTVarsTerm (i , m , bdy)

-- Convert every ParserTVar that appears in a Term into a usable integer TVar.
-- The integer parameter is the next type variable name to use, and the map
-- stores the existing equivalences between string type variables and integer
-- type variables.
convertTVarsTerm ::
    (Int, M.Map String Int, Term) -> (Int, M.Map String Int, Term)
convertTVarsTerm (i, map, exp) = case exp of

    Abs x t m  -> (i'', map'', Abs x t' m')
         where (i' , map' , m') = convertTVarsTerm (i , map , m)
               (i'', map'', t') = convertTVarsType (i', map', t)

    AbsInf x m -> (i' , map' , AbsInf x m')
         where (i' , map' , m') = convertTVarsTerm (i , map , m)

    App m n    -> (i'', map'', App m' n')
         where (i' , map' , m') = convertTVarsTerm (i , map , m)
               (i'', map'', n') = convertTVarsTerm (i', map', n)

    _ -> (i, map, exp)

-- Convert all ParserTVars into proper integer TVars. Returns a map from the
-- integer type variables to the user's string type variables that can be used
-- to print better error messages.
convertTVarsType ::
    (Int, M.Map String Int, Type) -> (Int, M.Map String Int, Type)
convertTVarsType (i, m, t) = case t of

    ParserTVar s -> case M.lookup s m of
                        Nothing -> (i + 1, M.insert s i m, TVar i)
                        Just j  -> (i    , m             , TVar j)

    TFunc t1 t2  -> (i''  , m'', TFunc t1' t2')
        where (i' , m' , t1') = convertTVarsType (i , m , t1)
              (i'', m'', t2') = convertTVarsType (i', m', t2)

    TProd t1 t2  -> (i''  , m'', TProd t1' t2')
        where (i' , m' , t1') = convertTVarsType (i , m , t1)
              (i'', m'', t2') = convertTVarsType (i', m', t2)

    TSum  t1 t2  -> (i''  , m'', TSum  t1' t2')
        where (i' , m' , t1') = convertTVarsType (i , m , t1)
              (i'', m'', t2') = convertTVarsType (i', m', t2)

    TList t      -> (i'   , m' , TList t'     )
        where (i' , m' , t' ) = convertTVarsType (i , m , t )

    TQuant _ _   -> error "convertTVars: TQuant found in parsed type"
    TVar   _     -> error "convertTVars: Integer TVar found in parsed type"
    other        -> (i, m, other)
