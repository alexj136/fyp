{-# OPTIONS_GHC -w #-}
module Parser where

import Lexer
import PolymorphicSyntax

{-- CONTEXT FREE GRAMMAR

DECLS       := DECLS DECL
            |  ε
DECL        := FUNC_NAME ARGLIST = EXP
ARGLIST     := ARGLIST ARG
            |  ε
EXP         := EXP EXP
            |  ID
            |  INT
            |  BINARYOP
            |  UNARYOP
INT         := [1-9][0-9]*
BINARYOP    := +
            |  -
            |  *
            |  div
            |  mod
UNARYOP     := iszero
            |  not
FUNC_NAME,
ID,
ARG         := [a-z][a-zA-Z]*

--}

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 ([Decl])
	| HappyAbsSyn5 (Decl)
	| HappyAbsSyn6 ([String])
	| HappyAbsSyn7 (TypedExp)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (13) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_2

action_1 (13) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail

action_2 (13) = happyShift action_3
action_2 (4) = happyGoto action_7
action_2 (5) = happyGoto action_2
action_2 _ = happyReduce_2

action_3 (13) = happyShift action_6
action_3 (6) = happyGoto action_5
action_3 _ = happyReduce_5

action_4 (16) = happyAccept
action_4 _ = happyFail

action_5 (12) = happyShift action_9
action_5 _ = happyFail

action_6 (13) = happyShift action_6
action_6 (6) = happyGoto action_8
action_6 _ = happyReduce_5

action_7 _ = happyReduce_1

action_8 _ = happyReduce_4

action_9 (8) = happyShift action_11
action_9 (10) = happyShift action_12
action_9 (13) = happyShift action_13
action_9 (14) = happyShift action_14
action_9 (15) = happyShift action_15
action_9 (7) = happyGoto action_10
action_9 _ = happyFail

action_10 (8) = happyShift action_11
action_10 (10) = happyShift action_12
action_10 (13) = happyShift action_13
action_10 (14) = happyShift action_14
action_10 (15) = happyShift action_15
action_10 (7) = happyGoto action_18
action_10 _ = happyReduce_3

action_11 (13) = happyShift action_17
action_11 _ = happyFail

action_12 (8) = happyShift action_11
action_12 (10) = happyShift action_12
action_12 (13) = happyShift action_13
action_12 (14) = happyShift action_14
action_12 (15) = happyShift action_15
action_12 (7) = happyGoto action_16
action_12 _ = happyFail

action_13 _ = happyReduce_9

action_14 _ = happyReduce_10

action_15 _ = happyReduce_11

action_16 (8) = happyShift action_11
action_16 (10) = happyShift action_12
action_16 (11) = happyShift action_20
action_16 (13) = happyShift action_13
action_16 (14) = happyShift action_14
action_16 (15) = happyShift action_15
action_16 (7) = happyGoto action_18
action_16 _ = happyFail

action_17 (9) = happyShift action_19
action_17 _ = happyFail

action_18 (8) = happyShift action_11
action_18 (10) = happyShift action_12
action_18 (13) = happyShift action_13
action_18 (14) = happyShift action_14
action_18 (15) = happyShift action_15
action_18 (7) = happyGoto action_18
action_18 _ = happyReduce_6

action_19 (8) = happyShift action_11
action_19 (10) = happyShift action_12
action_19 (13) = happyShift action_13
action_19 (14) = happyShift action_14
action_19 (15) = happyShift action_15
action_19 (7) = happyGoto action_21
action_19 _ = happyFail

action_20 _ = happyReduce_7

action_21 (8) = happyShift action_11
action_21 (10) = happyShift action_12
action_21 (13) = happyShift action_13
action_21 (14) = happyShift action_14
action_21 (15) = happyShift action_15
action_21 (7) = happyGoto action_18
action_21 _ = happyReduce_8

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 : happy_var_2
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  4 happyReduction_2
happyReduction_2  =  HappyAbsSyn4
		 ([]
	)

happyReduce_3 = happyReduce 4 5 happyReduction_3
happyReduction_3 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	(HappyTerminal (TokenIdLC happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (makeDecl happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_2  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_2)
	(HappyTerminal (TokenIdLC happy_var_1))
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_2
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_0  6 happyReduction_5
happyReduction_5  =  HappyAbsSyn6
		 ([]
	)

happyReduce_6 = happySpecReduce_2  7 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (App happy_var_1 happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 4 7 happyReduction_8
happyReduction_8 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdLC happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (AbsInf happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_1  7 happyReduction_9
happyReduction_9 (HappyTerminal (TokenIdLC happy_var_1))
	 =  HappyAbsSyn7
		 (Var happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  7 happyReduction_10
happyReduction_10 (HappyTerminal (TokenInt  happy_var_1))
	 =  HappyAbsSyn7
		 (Constant (IntVal happy_var_1)
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  7 happyReduction_11
happyReduction_11 (HappyTerminal (TokenBool happy_var_1))
	 =  HappyAbsSyn7
		 (Constant (BoolVal happy_var_1)
	)
happyReduction_11 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 16 16 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenLambda -> cont 8;
	TokenDot -> cont 9;
	TokenOpenBr -> cont 10;
	TokenClosBr -> cont 11;
	TokenEquals -> cont 12;
	TokenIdLC happy_dollar_dollar -> cont 13;
	TokenInt  happy_dollar_dollar -> cont 14;
	TokenBool happy_dollar_dollar -> cont 15;
	_ -> happyError' (tk:tks)
	}

happyError_ 16 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


data Decl = Decl {    -- A standard function declaration
    name  :: String,  -- The name if the function
    body  :: TypedExp -- The body of the function
} | DeclWTy {         -- For use with given type declarations (not yet implemented)
    name  :: String,
    tyDec :: Type,    -- The user-specified type, which we will check
    body  :: TypedExp
} deriving (Show, Eq)

makeDecl :: String -> [String] -> TypedExp -> Decl
makeDecl n []   x = Decl n x
makeDecl n args x = makeDecl n (init args) (AbsInf (last args) x)

makeDeclWTy :: Type -> String -> [String] -> TypedExp -> Decl
makeDeclWTy t n []   x = DeclWTy n t x
makeDeclWTy t n args x = makeDeclWTy t n (init args) (AbsInf (last args) x)

parseError :: [Token] -> a
parseError token = error $ "Parse error on " ++ (show token)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}





# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4























# 5 "<command-line>" 2
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
