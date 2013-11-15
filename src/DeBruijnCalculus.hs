module DeBruijnCalculus where

-- Info is not used for anything useful yet, but later it will be used to make
-- error messages more helpful
data Term = Var Info Int
          | App Info Term Term
          | Abs Info Name Term -- The name field is just a 'hint' for printing
                               -- convenience, it isn't actually used for
                               -- reduction etc.

-- A variable name is just a String
type Name = String

-- A binding at this point is just a NameBind, this will become more
-- sophisticated later
data Binding = NameBind

-- A Context stores information about bound variables, it is a list of
-- Name-Binding pairs
type Context = [(Name, Binding)]

-- Info will become more sophisticated later, at the moment it does nothing
data Info = Nil

instance Show Term where show t = showTerm [] t

data STerm = ST Context Term
instance Show STerm where show (ST c t) = showTerm c t

-- The real show function for Terms. Used by the instantiaition of Show by Term
showTerm :: Context -> Term -> String
showTerm ctx t = case t of
    Var _ x   -> fst (ctx !! x)
    App _ m n -> '(' : showTerm ctx m ++ ' ' : showTerm ctx n ++ ")"
    Abs _ v m -> 'λ' : v' ++ "." ++ showTerm ctx' m
        where (ctx', v') = freshName ctx v
              freshName :: Context -> Name -> (Context, Name)
              freshName ctx n = if nameIn ctx n then freshName ctx n'
                                else ((n', NameBind) : ctx, n')
                  where n' :: Name
                        n' = n ++ "'"
              nameIn :: Context -> Name -> Bool
              nameIn []    _ = False
              nameIn (h:t) n = if fst (h) == n then True else nameIn t n

-- 'Shifts' the bindings in a Term, that is, updates the binding values in
-- free variables, such that when an abstraction is removed (when reducing)
-- the numbers still refer to the correct bindings.
shift :: Int -> Term -> Term
shift places term = shift' 0 term
    where shift' :: Int -> Term -> Term
          shift' curPlace tm = case tm of
              Var i x | x >= curPlace -> Var i (x + places)
                      | otherwise     -> Var i x
              App i m n               -> App i (nextShift' m) (nextShift' n)
              Abs i v m               -> Abs i v (nextShift' m)
              where nextShift' :: Term -> Term
                    nextShift' = shift' (curPlace + 1)
