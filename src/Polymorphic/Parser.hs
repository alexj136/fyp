{-# OPTIONS_GHC -w #-}
module Parser where

import Lexer
import Syntax

{-- CONTEXT FREE GRAMMAR

PROG ::= PROG TYDC
       | PROG ID ARGS = EXP
       | epsilon
TYDC ::= TY :
       | epsilon
TY   ::= Int | Bool | Char
       | [ TY ]
       | { TY | TY }
       | { TY & TY }
       | TY -> TY
       | ( TY )
       | ID
ARGS ::= ID ARGS
       | epsilon
EXP  ::= let ID = EXP in EXP
       | LM ID . EXP
       | EXP EXP
       | ID
       | if EXP then EXP else EXP
       | *   | +  | -   | /   | %
       | ==  | <= | >=  | /=  | <      | >
       | and | or | not | xor | iszero
       | [] | [ EXP ] | [ EXP , ... , EXP ]
       | ( EXP )
       | { _ , EXP } | { EXP , _ } | { EXP , EXP }
       | INT
LM   ::= λ | ^ | <backslash>
ID   ::= [a-z][a-zA-Z]*
INT  ::= 0 | [1-9][0-9]*
BOOL ::= true | false
CHAR ::= 'a' | 'b' | 'c' | ...
STR  ::= "*"

--}

-- parser produced by Happy Version 1.19.3

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn6 (Prog)
	| HappyAbsSyn7 (Maybe Type)
	| HappyAbsSyn8 ([String])
	| HappyAbsSyn9 (Type)
	| HappyAbsSyn10 (Term)

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
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (14) = happyShift action_6
action_0 (16) = happyShift action_7
action_0 (18) = happyShift action_8
action_0 (54) = happyShift action_9
action_0 (55) = happyShift action_10
action_0 (56) = happyShift action_11
action_0 (57) = happyShift action_12
action_0 (64) = happyShift action_13
action_0 (6) = happyGoto action_36
action_0 (7) = happyGoto action_37
action_0 (9) = happyGoto action_5
action_0 _ = happyFail

action_1 (14) = happyShift action_17
action_1 (16) = happyShift action_18
action_1 (18) = happyShift action_19
action_1 (24) = happyShift action_20
action_1 (39) = happyShift action_21
action_1 (41) = happyShift action_22
action_1 (42) = happyShift action_23
action_1 (44) = happyShift action_24
action_1 (47) = happyShift action_25
action_1 (48) = happyShift action_26
action_1 (49) = happyShift action_27
action_1 (50) = happyShift action_28
action_1 (51) = happyShift action_29
action_1 (52) = happyShift action_30
action_1 (53) = happyShift action_31
action_1 (61) = happyShift action_32
action_1 (62) = happyShift action_33
action_1 (63) = happyShift action_34
action_1 (64) = happyShift action_35
action_1 (10) = happyGoto action_15
action_1 (11) = happyGoto action_16
action_1 _ = happyFail

action_2 (14) = happyShift action_6
action_2 (16) = happyShift action_7
action_2 (18) = happyShift action_8
action_2 (54) = happyShift action_9
action_2 (55) = happyShift action_10
action_2 (56) = happyShift action_11
action_2 (57) = happyShift action_12
action_2 (64) = happyShift action_13
action_2 (9) = happyGoto action_14
action_2 _ = happyFail

action_3 (14) = happyShift action_6
action_3 (16) = happyShift action_7
action_3 (18) = happyShift action_8
action_3 (54) = happyShift action_9
action_3 (55) = happyShift action_10
action_3 (56) = happyShift action_11
action_3 (57) = happyShift action_12
action_3 (64) = happyShift action_13
action_3 (7) = happyGoto action_4
action_3 (9) = happyGoto action_5
action_3 _ = happyFail

action_4 (64) = happyShift action_68
action_4 _ = happyFail

action_5 (22) = happyShift action_67
action_5 (60) = happyShift action_63
action_5 _ = happyFail

action_6 (14) = happyShift action_6
action_6 (16) = happyShift action_7
action_6 (18) = happyShift action_8
action_6 (54) = happyShift action_9
action_6 (55) = happyShift action_10
action_6 (56) = happyShift action_11
action_6 (57) = happyShift action_12
action_6 (64) = happyShift action_13
action_6 (9) = happyGoto action_66
action_6 _ = happyFail

action_7 (14) = happyShift action_6
action_7 (16) = happyShift action_7
action_7 (18) = happyShift action_8
action_7 (54) = happyShift action_9
action_7 (55) = happyShift action_10
action_7 (56) = happyShift action_11
action_7 (57) = happyShift action_12
action_7 (64) = happyShift action_13
action_7 (9) = happyGoto action_65
action_7 _ = happyFail

action_8 (14) = happyShift action_6
action_8 (16) = happyShift action_7
action_8 (18) = happyShift action_8
action_8 (54) = happyShift action_9
action_8 (55) = happyShift action_10
action_8 (56) = happyShift action_11
action_8 (57) = happyShift action_12
action_8 (64) = happyShift action_13
action_8 (9) = happyGoto action_64
action_8 _ = happyFail

action_9 _ = happyReduce_9

action_10 _ = happyReduce_10

action_11 _ = happyReduce_11

action_12 _ = happyReduce_12

action_13 _ = happyReduce_18

action_14 (60) = happyShift action_63
action_14 (66) = happyAccept
action_14 _ = happyFail

action_15 (14) = happyShift action_17
action_15 (16) = happyShift action_18
action_15 (18) = happyShift action_19
action_15 (24) = happyShift action_20
action_15 (26) = happyShift action_49
action_15 (27) = happyShift action_50
action_15 (28) = happyShift action_51
action_15 (29) = happyShift action_52
action_15 (30) = happyShift action_53
action_15 (31) = happyShift action_54
action_15 (32) = happyShift action_55
action_15 (33) = happyShift action_56
action_15 (34) = happyShift action_57
action_15 (35) = happyShift action_58
action_15 (36) = happyShift action_59
action_15 (37) = happyShift action_60
action_15 (38) = happyShift action_61
action_15 (39) = happyShift action_21
action_15 (40) = happyShift action_62
action_15 (41) = happyShift action_22
action_15 (42) = happyShift action_23
action_15 (44) = happyShift action_24
action_15 (47) = happyShift action_25
action_15 (48) = happyShift action_26
action_15 (49) = happyShift action_27
action_15 (50) = happyShift action_28
action_15 (51) = happyShift action_29
action_15 (52) = happyShift action_30
action_15 (53) = happyShift action_31
action_15 (61) = happyShift action_32
action_15 (62) = happyShift action_33
action_15 (63) = happyShift action_34
action_15 (64) = happyShift action_35
action_15 (66) = happyAccept
action_15 (10) = happyGoto action_47
action_15 (11) = happyGoto action_16
action_15 (13) = happyGoto action_48
action_15 _ = happyFail

action_16 _ = happyReduce_34

action_17 (14) = happyShift action_17
action_17 (16) = happyShift action_18
action_17 (18) = happyShift action_19
action_17 (24) = happyShift action_20
action_17 (39) = happyShift action_21
action_17 (41) = happyShift action_22
action_17 (42) = happyShift action_23
action_17 (44) = happyShift action_24
action_17 (47) = happyShift action_25
action_17 (48) = happyShift action_26
action_17 (49) = happyShift action_27
action_17 (50) = happyShift action_28
action_17 (51) = happyShift action_29
action_17 (52) = happyShift action_30
action_17 (53) = happyShift action_31
action_17 (61) = happyShift action_32
action_17 (62) = happyShift action_33
action_17 (63) = happyShift action_34
action_17 (64) = happyShift action_35
action_17 (10) = happyGoto action_46
action_17 (11) = happyGoto action_16
action_17 _ = happyFail

action_18 (14) = happyShift action_17
action_18 (16) = happyShift action_18
action_18 (17) = happyShift action_45
action_18 (18) = happyShift action_19
action_18 (24) = happyShift action_20
action_18 (39) = happyShift action_21
action_18 (41) = happyShift action_22
action_18 (42) = happyShift action_23
action_18 (44) = happyShift action_24
action_18 (47) = happyShift action_25
action_18 (48) = happyShift action_26
action_18 (49) = happyShift action_27
action_18 (50) = happyShift action_28
action_18 (51) = happyShift action_29
action_18 (52) = happyShift action_30
action_18 (53) = happyShift action_31
action_18 (61) = happyShift action_32
action_18 (62) = happyShift action_33
action_18 (63) = happyShift action_34
action_18 (64) = happyShift action_35
action_18 (10) = happyGoto action_44
action_18 (11) = happyGoto action_16
action_18 _ = happyFail

action_19 (14) = happyShift action_17
action_19 (16) = happyShift action_18
action_19 (18) = happyShift action_19
action_19 (23) = happyShift action_43
action_19 (24) = happyShift action_20
action_19 (39) = happyShift action_21
action_19 (41) = happyShift action_22
action_19 (42) = happyShift action_23
action_19 (44) = happyShift action_24
action_19 (47) = happyShift action_25
action_19 (48) = happyShift action_26
action_19 (49) = happyShift action_27
action_19 (50) = happyShift action_28
action_19 (51) = happyShift action_29
action_19 (52) = happyShift action_30
action_19 (53) = happyShift action_31
action_19 (61) = happyShift action_32
action_19 (62) = happyShift action_33
action_19 (63) = happyShift action_34
action_19 (64) = happyShift action_35
action_19 (10) = happyGoto action_42
action_19 (11) = happyGoto action_16
action_19 _ = happyFail

action_20 (64) = happyShift action_41
action_20 _ = happyFail

action_21 _ = happyReduce_26

action_22 _ = happyReduce_25

action_23 (64) = happyShift action_40
action_23 _ = happyFail

action_24 (14) = happyShift action_17
action_24 (16) = happyShift action_18
action_24 (18) = happyShift action_19
action_24 (24) = happyShift action_20
action_24 (39) = happyShift action_21
action_24 (41) = happyShift action_22
action_24 (42) = happyShift action_23
action_24 (44) = happyShift action_24
action_24 (47) = happyShift action_25
action_24 (48) = happyShift action_26
action_24 (49) = happyShift action_27
action_24 (50) = happyShift action_28
action_24 (51) = happyShift action_29
action_24 (52) = happyShift action_30
action_24 (53) = happyShift action_31
action_24 (61) = happyShift action_32
action_24 (62) = happyShift action_33
action_24 (63) = happyShift action_34
action_24 (64) = happyShift action_35
action_24 (10) = happyGoto action_39
action_24 (11) = happyGoto action_16
action_24 _ = happyFail

action_25 _ = happyReduce_27

action_26 _ = happyReduce_28

action_27 _ = happyReduce_29

action_28 _ = happyReduce_30

action_29 _ = happyReduce_31

action_30 _ = happyReduce_32

action_31 _ = happyReduce_33

action_32 _ = happyReduce_39

action_33 _ = happyReduce_40

action_34 _ = happyReduce_41

action_35 _ = happyReduce_22

action_36 (66) = happyAccept
action_36 _ = happyFail

action_37 (64) = happyShift action_38
action_37 _ = happyFail

action_38 (64) = happyShift action_70
action_38 (8) = happyGoto action_86
action_38 _ = happyReduce_8

action_39 (14) = happyShift action_17
action_39 (16) = happyShift action_18
action_39 (18) = happyShift action_19
action_39 (24) = happyShift action_20
action_39 (26) = happyShift action_49
action_39 (27) = happyShift action_50
action_39 (28) = happyShift action_51
action_39 (29) = happyShift action_52
action_39 (30) = happyShift action_53
action_39 (31) = happyShift action_54
action_39 (32) = happyShift action_55
action_39 (33) = happyShift action_56
action_39 (34) = happyShift action_57
action_39 (35) = happyShift action_58
action_39 (36) = happyShift action_59
action_39 (37) = happyShift action_60
action_39 (38) = happyShift action_61
action_39 (39) = happyShift action_21
action_39 (40) = happyShift action_62
action_39 (41) = happyShift action_22
action_39 (42) = happyShift action_23
action_39 (44) = happyShift action_24
action_39 (45) = happyShift action_85
action_39 (47) = happyShift action_25
action_39 (48) = happyShift action_26
action_39 (49) = happyShift action_27
action_39 (50) = happyShift action_28
action_39 (51) = happyShift action_29
action_39 (52) = happyShift action_30
action_39 (53) = happyShift action_31
action_39 (61) = happyShift action_32
action_39 (62) = happyShift action_33
action_39 (63) = happyShift action_34
action_39 (64) = happyShift action_35
action_39 (10) = happyGoto action_47
action_39 (11) = happyGoto action_16
action_39 (13) = happyGoto action_48
action_39 _ = happyFail

action_40 (20) = happyShift action_84
action_40 _ = happyFail

action_41 (25) = happyShift action_83
action_41 _ = happyFail

action_42 (14) = happyShift action_17
action_42 (16) = happyShift action_18
action_42 (18) = happyShift action_19
action_42 (21) = happyShift action_82
action_42 (24) = happyShift action_20
action_42 (26) = happyShift action_49
action_42 (27) = happyShift action_50
action_42 (28) = happyShift action_51
action_42 (29) = happyShift action_52
action_42 (30) = happyShift action_53
action_42 (31) = happyShift action_54
action_42 (32) = happyShift action_55
action_42 (33) = happyShift action_56
action_42 (34) = happyShift action_57
action_42 (35) = happyShift action_58
action_42 (36) = happyShift action_59
action_42 (37) = happyShift action_60
action_42 (38) = happyShift action_61
action_42 (39) = happyShift action_21
action_42 (40) = happyShift action_62
action_42 (41) = happyShift action_22
action_42 (42) = happyShift action_23
action_42 (44) = happyShift action_24
action_42 (47) = happyShift action_25
action_42 (48) = happyShift action_26
action_42 (49) = happyShift action_27
action_42 (50) = happyShift action_28
action_42 (51) = happyShift action_29
action_42 (52) = happyShift action_30
action_42 (53) = happyShift action_31
action_42 (61) = happyShift action_32
action_42 (62) = happyShift action_33
action_42 (63) = happyShift action_34
action_42 (64) = happyShift action_35
action_42 (10) = happyGoto action_47
action_42 (11) = happyGoto action_16
action_42 (13) = happyGoto action_48
action_42 _ = happyFail

action_43 (21) = happyShift action_81
action_43 _ = happyFail

action_44 (14) = happyShift action_17
action_44 (16) = happyShift action_18
action_44 (17) = happyShift action_79
action_44 (18) = happyShift action_19
action_44 (21) = happyShift action_80
action_44 (24) = happyShift action_20
action_44 (26) = happyShift action_49
action_44 (27) = happyShift action_50
action_44 (28) = happyShift action_51
action_44 (29) = happyShift action_52
action_44 (30) = happyShift action_53
action_44 (31) = happyShift action_54
action_44 (32) = happyShift action_55
action_44 (33) = happyShift action_56
action_44 (34) = happyShift action_57
action_44 (35) = happyShift action_58
action_44 (36) = happyShift action_59
action_44 (37) = happyShift action_60
action_44 (38) = happyShift action_61
action_44 (39) = happyShift action_21
action_44 (40) = happyShift action_62
action_44 (41) = happyShift action_22
action_44 (42) = happyShift action_23
action_44 (44) = happyShift action_24
action_44 (47) = happyShift action_25
action_44 (48) = happyShift action_26
action_44 (49) = happyShift action_27
action_44 (50) = happyShift action_28
action_44 (51) = happyShift action_29
action_44 (52) = happyShift action_30
action_44 (53) = happyShift action_31
action_44 (61) = happyShift action_32
action_44 (62) = happyShift action_33
action_44 (63) = happyShift action_34
action_44 (64) = happyShift action_35
action_44 (10) = happyGoto action_47
action_44 (11) = happyGoto action_16
action_44 (12) = happyGoto action_78
action_44 (13) = happyGoto action_48
action_44 _ = happyFail

action_45 _ = happyReduce_42

action_46 (14) = happyShift action_17
action_46 (15) = happyShift action_77
action_46 (16) = happyShift action_18
action_46 (18) = happyShift action_19
action_46 (24) = happyShift action_20
action_46 (26) = happyShift action_49
action_46 (27) = happyShift action_50
action_46 (28) = happyShift action_51
action_46 (29) = happyShift action_52
action_46 (30) = happyShift action_53
action_46 (31) = happyShift action_54
action_46 (32) = happyShift action_55
action_46 (33) = happyShift action_56
action_46 (34) = happyShift action_57
action_46 (35) = happyShift action_58
action_46 (36) = happyShift action_59
action_46 (37) = happyShift action_60
action_46 (38) = happyShift action_61
action_46 (39) = happyShift action_21
action_46 (40) = happyShift action_62
action_46 (41) = happyShift action_22
action_46 (42) = happyShift action_23
action_46 (44) = happyShift action_24
action_46 (47) = happyShift action_25
action_46 (48) = happyShift action_26
action_46 (49) = happyShift action_27
action_46 (50) = happyShift action_28
action_46 (51) = happyShift action_29
action_46 (52) = happyShift action_30
action_46 (53) = happyShift action_31
action_46 (61) = happyShift action_32
action_46 (62) = happyShift action_33
action_46 (63) = happyShift action_34
action_46 (64) = happyShift action_35
action_46 (10) = happyGoto action_47
action_46 (11) = happyGoto action_16
action_46 (13) = happyGoto action_48
action_46 _ = happyFail

action_47 (24) = happyShift action_20
action_47 (39) = happyShift action_21
action_47 (40) = happyShift action_62
action_47 (41) = happyShift action_22
action_47 (44) = happyShift action_24
action_47 (47) = happyShift action_25
action_47 (48) = happyShift action_26
action_47 (49) = happyShift action_27
action_47 (50) = happyShift action_28
action_47 (51) = happyShift action_29
action_47 (52) = happyShift action_30
action_47 (53) = happyShift action_31
action_47 (63) = happyShift action_34
action_47 (10) = happyGoto action_47
action_47 (11) = happyGoto action_16
action_47 (13) = happyGoto action_48
action_47 _ = happyReduce_21

action_48 (14) = happyShift action_17
action_48 (16) = happyShift action_18
action_48 (18) = happyShift action_19
action_48 (24) = happyShift action_20
action_48 (39) = happyShift action_21
action_48 (41) = happyShift action_22
action_48 (42) = happyShift action_23
action_48 (44) = happyShift action_24
action_48 (47) = happyShift action_25
action_48 (48) = happyShift action_26
action_48 (49) = happyShift action_27
action_48 (50) = happyShift action_28
action_48 (51) = happyShift action_29
action_48 (52) = happyShift action_30
action_48 (53) = happyShift action_31
action_48 (61) = happyShift action_32
action_48 (62) = happyShift action_33
action_48 (63) = happyShift action_34
action_48 (64) = happyShift action_35
action_48 (10) = happyGoto action_76
action_48 (11) = happyGoto action_16
action_48 _ = happyFail

action_49 _ = happyReduce_46

action_50 _ = happyReduce_47

action_51 _ = happyReduce_48

action_52 _ = happyReduce_49

action_53 _ = happyReduce_50

action_54 _ = happyReduce_56

action_55 _ = happyReduce_55

action_56 _ = happyReduce_54

action_57 _ = happyReduce_59

action_58 _ = happyReduce_58

action_59 _ = happyReduce_57

action_60 _ = happyReduce_51

action_61 _ = happyReduce_52

action_62 _ = happyReduce_53

action_63 (14) = happyShift action_6
action_63 (16) = happyShift action_7
action_63 (18) = happyShift action_8
action_63 (54) = happyShift action_9
action_63 (55) = happyShift action_10
action_63 (56) = happyShift action_11
action_63 (57) = happyShift action_12
action_63 (64) = happyShift action_13
action_63 (9) = happyGoto action_75
action_63 _ = happyFail

action_64 (58) = happyShift action_73
action_64 (59) = happyShift action_74
action_64 (60) = happyShift action_63
action_64 _ = happyFail

action_65 (17) = happyShift action_72
action_65 (60) = happyShift action_63
action_65 _ = happyFail

action_66 (15) = happyShift action_71
action_66 (60) = happyShift action_63
action_66 _ = happyFail

action_67 _ = happyReduce_5

action_68 (64) = happyShift action_70
action_68 (8) = happyGoto action_69
action_68 _ = happyFail

action_69 (20) = happyShift action_98
action_69 _ = happyFail

action_70 (64) = happyShift action_70
action_70 (8) = happyGoto action_97
action_70 _ = happyReduce_8

action_71 _ = happyReduce_17

action_72 _ = happyReduce_13

action_73 (14) = happyShift action_6
action_73 (16) = happyShift action_7
action_73 (18) = happyShift action_8
action_73 (54) = happyShift action_9
action_73 (55) = happyShift action_10
action_73 (56) = happyShift action_11
action_73 (57) = happyShift action_12
action_73 (64) = happyShift action_13
action_73 (9) = happyGoto action_96
action_73 _ = happyFail

action_74 (14) = happyShift action_6
action_74 (16) = happyShift action_7
action_74 (18) = happyShift action_8
action_74 (54) = happyShift action_9
action_74 (55) = happyShift action_10
action_74 (56) = happyShift action_11
action_74 (57) = happyShift action_12
action_74 (64) = happyShift action_13
action_74 (9) = happyGoto action_95
action_74 _ = happyFail

action_75 (60) = happyShift action_63
action_75 _ = happyReduce_16

action_76 (14) = happyShift action_17
action_76 (16) = happyShift action_18
action_76 (18) = happyShift action_19
action_76 (24) = happyShift action_20
action_76 (26) = happyShift action_49
action_76 (27) = happyShift action_50
action_76 (28) = happyShift action_51
action_76 (29) = happyShift action_52
action_76 (30) = happyShift action_53
action_76 (31) = happyShift action_54
action_76 (32) = happyShift action_55
action_76 (33) = happyShift action_56
action_76 (34) = happyShift action_57
action_76 (35) = happyShift action_58
action_76 (36) = happyShift action_59
action_76 (37) = happyShift action_60
action_76 (38) = happyShift action_61
action_76 (39) = happyShift action_21
action_76 (40) = happyShift action_62
action_76 (41) = happyShift action_22
action_76 (42) = happyShift action_23
action_76 (44) = happyShift action_24
action_76 (47) = happyShift action_25
action_76 (48) = happyShift action_26
action_76 (49) = happyShift action_27
action_76 (50) = happyShift action_28
action_76 (51) = happyShift action_29
action_76 (52) = happyShift action_30
action_76 (53) = happyShift action_31
action_76 (61) = happyShift action_32
action_76 (62) = happyShift action_33
action_76 (63) = happyShift action_34
action_76 (64) = happyShift action_35
action_76 (10) = happyGoto action_47
action_76 (11) = happyGoto action_16
action_76 (13) = happyGoto action_48
action_76 _ = happyReduce_24

action_77 _ = happyReduce_35

action_78 _ = happyReduce_43

action_79 _ = happyReduce_45

action_80 (14) = happyShift action_17
action_80 (16) = happyShift action_18
action_80 (18) = happyShift action_19
action_80 (24) = happyShift action_20
action_80 (39) = happyShift action_21
action_80 (41) = happyShift action_22
action_80 (42) = happyShift action_23
action_80 (44) = happyShift action_24
action_80 (47) = happyShift action_25
action_80 (48) = happyShift action_26
action_80 (49) = happyShift action_27
action_80 (50) = happyShift action_28
action_80 (51) = happyShift action_29
action_80 (52) = happyShift action_30
action_80 (53) = happyShift action_31
action_80 (61) = happyShift action_32
action_80 (62) = happyShift action_33
action_80 (63) = happyShift action_34
action_80 (64) = happyShift action_35
action_80 (10) = happyGoto action_94
action_80 (11) = happyGoto action_16
action_80 _ = happyFail

action_81 (14) = happyShift action_17
action_81 (16) = happyShift action_18
action_81 (18) = happyShift action_19
action_81 (24) = happyShift action_20
action_81 (39) = happyShift action_21
action_81 (41) = happyShift action_22
action_81 (42) = happyShift action_23
action_81 (44) = happyShift action_24
action_81 (47) = happyShift action_25
action_81 (48) = happyShift action_26
action_81 (49) = happyShift action_27
action_81 (50) = happyShift action_28
action_81 (51) = happyShift action_29
action_81 (52) = happyShift action_30
action_81 (53) = happyShift action_31
action_81 (61) = happyShift action_32
action_81 (62) = happyShift action_33
action_81 (63) = happyShift action_34
action_81 (64) = happyShift action_35
action_81 (10) = happyGoto action_93
action_81 (11) = happyGoto action_16
action_81 _ = happyFail

action_82 (14) = happyShift action_17
action_82 (16) = happyShift action_18
action_82 (18) = happyShift action_19
action_82 (23) = happyShift action_92
action_82 (24) = happyShift action_20
action_82 (39) = happyShift action_21
action_82 (41) = happyShift action_22
action_82 (42) = happyShift action_23
action_82 (44) = happyShift action_24
action_82 (47) = happyShift action_25
action_82 (48) = happyShift action_26
action_82 (49) = happyShift action_27
action_82 (50) = happyShift action_28
action_82 (51) = happyShift action_29
action_82 (52) = happyShift action_30
action_82 (53) = happyShift action_31
action_82 (61) = happyShift action_32
action_82 (62) = happyShift action_33
action_82 (63) = happyShift action_34
action_82 (64) = happyShift action_35
action_82 (10) = happyGoto action_91
action_82 (11) = happyGoto action_16
action_82 _ = happyFail

action_83 (14) = happyShift action_17
action_83 (16) = happyShift action_18
action_83 (18) = happyShift action_19
action_83 (24) = happyShift action_20
action_83 (39) = happyShift action_21
action_83 (41) = happyShift action_22
action_83 (42) = happyShift action_23
action_83 (44) = happyShift action_24
action_83 (47) = happyShift action_25
action_83 (48) = happyShift action_26
action_83 (49) = happyShift action_27
action_83 (50) = happyShift action_28
action_83 (51) = happyShift action_29
action_83 (52) = happyShift action_30
action_83 (53) = happyShift action_31
action_83 (61) = happyShift action_32
action_83 (62) = happyShift action_33
action_83 (63) = happyShift action_34
action_83 (64) = happyShift action_35
action_83 (10) = happyGoto action_90
action_83 (11) = happyGoto action_16
action_83 _ = happyFail

action_84 (14) = happyShift action_17
action_84 (16) = happyShift action_18
action_84 (18) = happyShift action_19
action_84 (24) = happyShift action_20
action_84 (39) = happyShift action_21
action_84 (41) = happyShift action_22
action_84 (42) = happyShift action_23
action_84 (44) = happyShift action_24
action_84 (47) = happyShift action_25
action_84 (48) = happyShift action_26
action_84 (49) = happyShift action_27
action_84 (50) = happyShift action_28
action_84 (51) = happyShift action_29
action_84 (52) = happyShift action_30
action_84 (53) = happyShift action_31
action_84 (61) = happyShift action_32
action_84 (62) = happyShift action_33
action_84 (63) = happyShift action_34
action_84 (64) = happyShift action_35
action_84 (10) = happyGoto action_89
action_84 (11) = happyGoto action_16
action_84 _ = happyFail

action_85 (14) = happyShift action_17
action_85 (16) = happyShift action_18
action_85 (18) = happyShift action_19
action_85 (24) = happyShift action_20
action_85 (39) = happyShift action_21
action_85 (41) = happyShift action_22
action_85 (42) = happyShift action_23
action_85 (44) = happyShift action_24
action_85 (47) = happyShift action_25
action_85 (48) = happyShift action_26
action_85 (49) = happyShift action_27
action_85 (50) = happyShift action_28
action_85 (51) = happyShift action_29
action_85 (52) = happyShift action_30
action_85 (53) = happyShift action_31
action_85 (61) = happyShift action_32
action_85 (62) = happyShift action_33
action_85 (63) = happyShift action_34
action_85 (64) = happyShift action_35
action_85 (10) = happyGoto action_88
action_85 (11) = happyGoto action_16
action_85 _ = happyFail

action_86 (20) = happyShift action_87
action_86 _ = happyFail

action_87 (14) = happyShift action_17
action_87 (16) = happyShift action_18
action_87 (18) = happyShift action_19
action_87 (24) = happyShift action_20
action_87 (39) = happyShift action_21
action_87 (41) = happyShift action_22
action_87 (42) = happyShift action_23
action_87 (44) = happyShift action_24
action_87 (47) = happyShift action_25
action_87 (48) = happyShift action_26
action_87 (49) = happyShift action_27
action_87 (50) = happyShift action_28
action_87 (51) = happyShift action_29
action_87 (52) = happyShift action_30
action_87 (53) = happyShift action_31
action_87 (61) = happyShift action_32
action_87 (62) = happyShift action_33
action_87 (63) = happyShift action_34
action_87 (64) = happyShift action_35
action_87 (10) = happyGoto action_108
action_87 (11) = happyGoto action_16
action_87 _ = happyFail

action_88 (14) = happyShift action_17
action_88 (16) = happyShift action_18
action_88 (18) = happyShift action_19
action_88 (24) = happyShift action_20
action_88 (26) = happyShift action_49
action_88 (27) = happyShift action_50
action_88 (28) = happyShift action_51
action_88 (29) = happyShift action_52
action_88 (30) = happyShift action_53
action_88 (31) = happyShift action_54
action_88 (32) = happyShift action_55
action_88 (33) = happyShift action_56
action_88 (34) = happyShift action_57
action_88 (35) = happyShift action_58
action_88 (36) = happyShift action_59
action_88 (37) = happyShift action_60
action_88 (38) = happyShift action_61
action_88 (39) = happyShift action_21
action_88 (40) = happyShift action_62
action_88 (41) = happyShift action_22
action_88 (42) = happyShift action_23
action_88 (44) = happyShift action_24
action_88 (46) = happyShift action_107
action_88 (47) = happyShift action_25
action_88 (48) = happyShift action_26
action_88 (49) = happyShift action_27
action_88 (50) = happyShift action_28
action_88 (51) = happyShift action_29
action_88 (52) = happyShift action_30
action_88 (53) = happyShift action_31
action_88 (61) = happyShift action_32
action_88 (62) = happyShift action_33
action_88 (63) = happyShift action_34
action_88 (64) = happyShift action_35
action_88 (10) = happyGoto action_47
action_88 (11) = happyGoto action_16
action_88 (13) = happyGoto action_48
action_88 _ = happyFail

action_89 (14) = happyShift action_17
action_89 (16) = happyShift action_18
action_89 (18) = happyShift action_19
action_89 (24) = happyShift action_20
action_89 (26) = happyShift action_49
action_89 (27) = happyShift action_50
action_89 (28) = happyShift action_51
action_89 (29) = happyShift action_52
action_89 (30) = happyShift action_53
action_89 (31) = happyShift action_54
action_89 (32) = happyShift action_55
action_89 (33) = happyShift action_56
action_89 (34) = happyShift action_57
action_89 (35) = happyShift action_58
action_89 (36) = happyShift action_59
action_89 (37) = happyShift action_60
action_89 (38) = happyShift action_61
action_89 (39) = happyShift action_21
action_89 (40) = happyShift action_62
action_89 (41) = happyShift action_22
action_89 (42) = happyShift action_23
action_89 (43) = happyShift action_106
action_89 (44) = happyShift action_24
action_89 (47) = happyShift action_25
action_89 (48) = happyShift action_26
action_89 (49) = happyShift action_27
action_89 (50) = happyShift action_28
action_89 (51) = happyShift action_29
action_89 (52) = happyShift action_30
action_89 (53) = happyShift action_31
action_89 (61) = happyShift action_32
action_89 (62) = happyShift action_33
action_89 (63) = happyShift action_34
action_89 (64) = happyShift action_35
action_89 (10) = happyGoto action_47
action_89 (11) = happyGoto action_16
action_89 (13) = happyGoto action_48
action_89 _ = happyFail

action_90 (24) = happyShift action_20
action_90 (26) = happyShift action_49
action_90 (27) = happyShift action_50
action_90 (28) = happyShift action_51
action_90 (29) = happyShift action_52
action_90 (30) = happyShift action_53
action_90 (31) = happyShift action_54
action_90 (32) = happyShift action_55
action_90 (33) = happyShift action_56
action_90 (34) = happyShift action_57
action_90 (35) = happyShift action_58
action_90 (36) = happyShift action_59
action_90 (37) = happyShift action_60
action_90 (38) = happyShift action_61
action_90 (39) = happyShift action_21
action_90 (40) = happyShift action_62
action_90 (41) = happyShift action_22
action_90 (42) = happyShift action_23
action_90 (44) = happyShift action_24
action_90 (47) = happyShift action_25
action_90 (48) = happyShift action_26
action_90 (49) = happyShift action_27
action_90 (50) = happyShift action_28
action_90 (51) = happyShift action_29
action_90 (52) = happyShift action_30
action_90 (53) = happyShift action_31
action_90 (61) = happyShift action_32
action_90 (62) = happyShift action_33
action_90 (63) = happyShift action_34
action_90 (64) = happyShift action_35
action_90 (10) = happyGoto action_47
action_90 (11) = happyGoto action_16
action_90 (13) = happyGoto action_48
action_90 _ = happyReduce_20

action_91 (14) = happyShift action_17
action_91 (16) = happyShift action_18
action_91 (18) = happyShift action_19
action_91 (19) = happyShift action_105
action_91 (24) = happyShift action_20
action_91 (26) = happyShift action_49
action_91 (27) = happyShift action_50
action_91 (28) = happyShift action_51
action_91 (29) = happyShift action_52
action_91 (30) = happyShift action_53
action_91 (31) = happyShift action_54
action_91 (32) = happyShift action_55
action_91 (33) = happyShift action_56
action_91 (34) = happyShift action_57
action_91 (35) = happyShift action_58
action_91 (36) = happyShift action_59
action_91 (37) = happyShift action_60
action_91 (38) = happyShift action_61
action_91 (39) = happyShift action_21
action_91 (40) = happyShift action_62
action_91 (41) = happyShift action_22
action_91 (42) = happyShift action_23
action_91 (44) = happyShift action_24
action_91 (47) = happyShift action_25
action_91 (48) = happyShift action_26
action_91 (49) = happyShift action_27
action_91 (50) = happyShift action_28
action_91 (51) = happyShift action_29
action_91 (52) = happyShift action_30
action_91 (53) = happyShift action_31
action_91 (61) = happyShift action_32
action_91 (62) = happyShift action_33
action_91 (63) = happyShift action_34
action_91 (64) = happyShift action_35
action_91 (10) = happyGoto action_47
action_91 (11) = happyGoto action_16
action_91 (13) = happyGoto action_48
action_91 _ = happyFail

action_92 (19) = happyShift action_104
action_92 _ = happyFail

action_93 (14) = happyShift action_17
action_93 (16) = happyShift action_18
action_93 (18) = happyShift action_19
action_93 (19) = happyShift action_103
action_93 (24) = happyShift action_20
action_93 (26) = happyShift action_49
action_93 (27) = happyShift action_50
action_93 (28) = happyShift action_51
action_93 (29) = happyShift action_52
action_93 (30) = happyShift action_53
action_93 (31) = happyShift action_54
action_93 (32) = happyShift action_55
action_93 (33) = happyShift action_56
action_93 (34) = happyShift action_57
action_93 (35) = happyShift action_58
action_93 (36) = happyShift action_59
action_93 (37) = happyShift action_60
action_93 (38) = happyShift action_61
action_93 (39) = happyShift action_21
action_93 (40) = happyShift action_62
action_93 (41) = happyShift action_22
action_93 (42) = happyShift action_23
action_93 (44) = happyShift action_24
action_93 (47) = happyShift action_25
action_93 (48) = happyShift action_26
action_93 (49) = happyShift action_27
action_93 (50) = happyShift action_28
action_93 (51) = happyShift action_29
action_93 (52) = happyShift action_30
action_93 (53) = happyShift action_31
action_93 (61) = happyShift action_32
action_93 (62) = happyShift action_33
action_93 (63) = happyShift action_34
action_93 (64) = happyShift action_35
action_93 (10) = happyGoto action_47
action_93 (11) = happyGoto action_16
action_93 (13) = happyGoto action_48
action_93 _ = happyFail

action_94 (14) = happyShift action_17
action_94 (16) = happyShift action_18
action_94 (17) = happyShift action_79
action_94 (18) = happyShift action_19
action_94 (21) = happyShift action_80
action_94 (24) = happyShift action_20
action_94 (26) = happyShift action_49
action_94 (27) = happyShift action_50
action_94 (28) = happyShift action_51
action_94 (29) = happyShift action_52
action_94 (30) = happyShift action_53
action_94 (31) = happyShift action_54
action_94 (32) = happyShift action_55
action_94 (33) = happyShift action_56
action_94 (34) = happyShift action_57
action_94 (35) = happyShift action_58
action_94 (36) = happyShift action_59
action_94 (37) = happyShift action_60
action_94 (38) = happyShift action_61
action_94 (39) = happyShift action_21
action_94 (40) = happyShift action_62
action_94 (41) = happyShift action_22
action_94 (42) = happyShift action_23
action_94 (44) = happyShift action_24
action_94 (47) = happyShift action_25
action_94 (48) = happyShift action_26
action_94 (49) = happyShift action_27
action_94 (50) = happyShift action_28
action_94 (51) = happyShift action_29
action_94 (52) = happyShift action_30
action_94 (53) = happyShift action_31
action_94 (61) = happyShift action_32
action_94 (62) = happyShift action_33
action_94 (63) = happyShift action_34
action_94 (64) = happyShift action_35
action_94 (10) = happyGoto action_47
action_94 (11) = happyGoto action_16
action_94 (12) = happyGoto action_102
action_94 (13) = happyGoto action_48
action_94 _ = happyFail

action_95 (19) = happyShift action_101
action_95 (60) = happyShift action_63
action_95 _ = happyFail

action_96 (19) = happyShift action_100
action_96 (60) = happyShift action_63
action_96 _ = happyFail

action_97 _ = happyReduce_7

action_98 (14) = happyShift action_17
action_98 (16) = happyShift action_18
action_98 (18) = happyShift action_19
action_98 (24) = happyShift action_20
action_98 (39) = happyShift action_21
action_98 (41) = happyShift action_22
action_98 (42) = happyShift action_23
action_98 (44) = happyShift action_24
action_98 (47) = happyShift action_25
action_98 (48) = happyShift action_26
action_98 (49) = happyShift action_27
action_98 (50) = happyShift action_28
action_98 (51) = happyShift action_29
action_98 (52) = happyShift action_30
action_98 (53) = happyShift action_31
action_98 (61) = happyShift action_32
action_98 (62) = happyShift action_33
action_98 (63) = happyShift action_34
action_98 (64) = happyShift action_35
action_98 (10) = happyGoto action_99
action_98 (11) = happyGoto action_16
action_98 _ = happyFail

action_99 (14) = happyShift action_110
action_99 (16) = happyShift action_111
action_99 (18) = happyShift action_112
action_99 (24) = happyShift action_20
action_99 (26) = happyShift action_49
action_99 (27) = happyShift action_50
action_99 (28) = happyShift action_51
action_99 (29) = happyShift action_52
action_99 (30) = happyShift action_53
action_99 (31) = happyShift action_54
action_99 (32) = happyShift action_55
action_99 (33) = happyShift action_56
action_99 (34) = happyShift action_57
action_99 (35) = happyShift action_58
action_99 (36) = happyShift action_59
action_99 (37) = happyShift action_60
action_99 (38) = happyShift action_61
action_99 (39) = happyShift action_21
action_99 (40) = happyShift action_62
action_99 (41) = happyShift action_22
action_99 (42) = happyShift action_23
action_99 (44) = happyShift action_24
action_99 (47) = happyShift action_25
action_99 (48) = happyShift action_26
action_99 (49) = happyShift action_27
action_99 (50) = happyShift action_28
action_99 (51) = happyShift action_29
action_99 (52) = happyShift action_30
action_99 (53) = happyShift action_31
action_99 (54) = happyShift action_9
action_99 (55) = happyShift action_10
action_99 (56) = happyShift action_11
action_99 (57) = happyShift action_12
action_99 (61) = happyShift action_32
action_99 (62) = happyShift action_33
action_99 (63) = happyShift action_34
action_99 (64) = happyShift action_113
action_99 (6) = happyGoto action_109
action_99 (7) = happyGoto action_37
action_99 (9) = happyGoto action_5
action_99 (10) = happyGoto action_47
action_99 (11) = happyGoto action_16
action_99 (13) = happyGoto action_48
action_99 _ = happyFail

action_100 _ = happyReduce_14

action_101 _ = happyReduce_15

action_102 _ = happyReduce_44

action_103 _ = happyReduce_37

action_104 _ = happyReduce_36

action_105 _ = happyReduce_38

action_106 (14) = happyShift action_17
action_106 (16) = happyShift action_18
action_106 (18) = happyShift action_19
action_106 (24) = happyShift action_20
action_106 (39) = happyShift action_21
action_106 (41) = happyShift action_22
action_106 (42) = happyShift action_23
action_106 (44) = happyShift action_24
action_106 (47) = happyShift action_25
action_106 (48) = happyShift action_26
action_106 (49) = happyShift action_27
action_106 (50) = happyShift action_28
action_106 (51) = happyShift action_29
action_106 (52) = happyShift action_30
action_106 (53) = happyShift action_31
action_106 (61) = happyShift action_32
action_106 (62) = happyShift action_33
action_106 (63) = happyShift action_34
action_106 (64) = happyShift action_35
action_106 (10) = happyGoto action_115
action_106 (11) = happyGoto action_16
action_106 _ = happyFail

action_107 (14) = happyShift action_17
action_107 (16) = happyShift action_18
action_107 (18) = happyShift action_19
action_107 (24) = happyShift action_20
action_107 (39) = happyShift action_21
action_107 (41) = happyShift action_22
action_107 (42) = happyShift action_23
action_107 (44) = happyShift action_24
action_107 (47) = happyShift action_25
action_107 (48) = happyShift action_26
action_107 (49) = happyShift action_27
action_107 (50) = happyShift action_28
action_107 (51) = happyShift action_29
action_107 (52) = happyShift action_30
action_107 (53) = happyShift action_31
action_107 (61) = happyShift action_32
action_107 (62) = happyShift action_33
action_107 (63) = happyShift action_34
action_107 (64) = happyShift action_35
action_107 (10) = happyGoto action_114
action_107 (11) = happyGoto action_16
action_107 _ = happyFail

action_108 (14) = happyShift action_110
action_108 (16) = happyShift action_111
action_108 (18) = happyShift action_112
action_108 (24) = happyShift action_20
action_108 (26) = happyShift action_49
action_108 (27) = happyShift action_50
action_108 (28) = happyShift action_51
action_108 (29) = happyShift action_52
action_108 (30) = happyShift action_53
action_108 (31) = happyShift action_54
action_108 (32) = happyShift action_55
action_108 (33) = happyShift action_56
action_108 (34) = happyShift action_57
action_108 (35) = happyShift action_58
action_108 (36) = happyShift action_59
action_108 (37) = happyShift action_60
action_108 (38) = happyShift action_61
action_108 (39) = happyShift action_21
action_108 (40) = happyShift action_62
action_108 (41) = happyShift action_22
action_108 (42) = happyShift action_23
action_108 (44) = happyShift action_24
action_108 (47) = happyShift action_25
action_108 (48) = happyShift action_26
action_108 (49) = happyShift action_27
action_108 (50) = happyShift action_28
action_108 (51) = happyShift action_29
action_108 (52) = happyShift action_30
action_108 (53) = happyShift action_31
action_108 (54) = happyShift action_9
action_108 (55) = happyShift action_10
action_108 (56) = happyShift action_11
action_108 (57) = happyShift action_12
action_108 (61) = happyShift action_32
action_108 (62) = happyShift action_33
action_108 (63) = happyShift action_34
action_108 (64) = happyShift action_113
action_108 (6) = happyGoto action_109
action_108 (7) = happyGoto action_37
action_108 (9) = happyGoto action_5
action_108 (10) = happyGoto action_47
action_108 (11) = happyGoto action_16
action_108 (13) = happyGoto action_48
action_108 _ = happyReduce_4

action_109 _ = happyReduce_3

action_110 (14) = happyShift action_110
action_110 (16) = happyShift action_111
action_110 (18) = happyShift action_112
action_110 (24) = happyShift action_20
action_110 (39) = happyShift action_21
action_110 (41) = happyShift action_22
action_110 (42) = happyShift action_23
action_110 (44) = happyShift action_24
action_110 (47) = happyShift action_25
action_110 (48) = happyShift action_26
action_110 (49) = happyShift action_27
action_110 (50) = happyShift action_28
action_110 (51) = happyShift action_29
action_110 (52) = happyShift action_30
action_110 (53) = happyShift action_31
action_110 (54) = happyShift action_9
action_110 (55) = happyShift action_10
action_110 (56) = happyShift action_11
action_110 (57) = happyShift action_12
action_110 (61) = happyShift action_32
action_110 (62) = happyShift action_33
action_110 (63) = happyShift action_34
action_110 (64) = happyShift action_113
action_110 (9) = happyGoto action_66
action_110 (10) = happyGoto action_46
action_110 (11) = happyGoto action_16
action_110 _ = happyFail

action_111 (14) = happyShift action_110
action_111 (16) = happyShift action_111
action_111 (17) = happyShift action_45
action_111 (18) = happyShift action_112
action_111 (24) = happyShift action_20
action_111 (39) = happyShift action_21
action_111 (41) = happyShift action_22
action_111 (42) = happyShift action_23
action_111 (44) = happyShift action_24
action_111 (47) = happyShift action_25
action_111 (48) = happyShift action_26
action_111 (49) = happyShift action_27
action_111 (50) = happyShift action_28
action_111 (51) = happyShift action_29
action_111 (52) = happyShift action_30
action_111 (53) = happyShift action_31
action_111 (54) = happyShift action_9
action_111 (55) = happyShift action_10
action_111 (56) = happyShift action_11
action_111 (57) = happyShift action_12
action_111 (61) = happyShift action_32
action_111 (62) = happyShift action_33
action_111 (63) = happyShift action_34
action_111 (64) = happyShift action_113
action_111 (9) = happyGoto action_65
action_111 (10) = happyGoto action_44
action_111 (11) = happyGoto action_16
action_111 _ = happyFail

action_112 (14) = happyShift action_110
action_112 (16) = happyShift action_111
action_112 (18) = happyShift action_112
action_112 (23) = happyShift action_43
action_112 (24) = happyShift action_20
action_112 (39) = happyShift action_21
action_112 (41) = happyShift action_22
action_112 (42) = happyShift action_23
action_112 (44) = happyShift action_24
action_112 (47) = happyShift action_25
action_112 (48) = happyShift action_26
action_112 (49) = happyShift action_27
action_112 (50) = happyShift action_28
action_112 (51) = happyShift action_29
action_112 (52) = happyShift action_30
action_112 (53) = happyShift action_31
action_112 (54) = happyShift action_9
action_112 (55) = happyShift action_10
action_112 (56) = happyShift action_11
action_112 (57) = happyShift action_12
action_112 (61) = happyShift action_32
action_112 (62) = happyShift action_33
action_112 (63) = happyShift action_34
action_112 (64) = happyShift action_113
action_112 (9) = happyGoto action_64
action_112 (10) = happyGoto action_42
action_112 (11) = happyGoto action_16
action_112 _ = happyFail

action_113 (15) = happyReduce_18
action_113 (17) = happyReduce_18
action_113 (22) = happyReduce_18
action_113 (58) = happyReduce_18
action_113 (59) = happyReduce_18
action_113 (60) = happyReduce_18
action_113 _ = happyReduce_22

action_114 (14) = happyShift action_17
action_114 (16) = happyShift action_18
action_114 (18) = happyShift action_19
action_114 (24) = happyShift action_20
action_114 (26) = happyShift action_49
action_114 (27) = happyShift action_50
action_114 (28) = happyShift action_51
action_114 (29) = happyShift action_52
action_114 (30) = happyShift action_53
action_114 (31) = happyShift action_54
action_114 (32) = happyShift action_55
action_114 (33) = happyShift action_56
action_114 (34) = happyShift action_57
action_114 (35) = happyShift action_58
action_114 (36) = happyShift action_59
action_114 (37) = happyShift action_60
action_114 (38) = happyShift action_61
action_114 (39) = happyShift action_21
action_114 (40) = happyShift action_62
action_114 (41) = happyShift action_22
action_114 (42) = happyShift action_23
action_114 (44) = happyShift action_24
action_114 (47) = happyShift action_25
action_114 (48) = happyShift action_26
action_114 (49) = happyShift action_27
action_114 (50) = happyShift action_28
action_114 (51) = happyShift action_29
action_114 (52) = happyShift action_30
action_114 (53) = happyShift action_31
action_114 (61) = happyShift action_32
action_114 (62) = happyShift action_33
action_114 (63) = happyShift action_34
action_114 (64) = happyShift action_35
action_114 (10) = happyGoto action_47
action_114 (11) = happyGoto action_16
action_114 (13) = happyGoto action_48
action_114 _ = happyReduce_23

action_115 (24) = happyShift action_20
action_115 (26) = happyShift action_49
action_115 (27) = happyShift action_50
action_115 (28) = happyShift action_51
action_115 (29) = happyShift action_52
action_115 (30) = happyShift action_53
action_115 (31) = happyShift action_54
action_115 (32) = happyShift action_55
action_115 (33) = happyShift action_56
action_115 (34) = happyShift action_57
action_115 (35) = happyShift action_58
action_115 (36) = happyShift action_59
action_115 (37) = happyShift action_60
action_115 (38) = happyShift action_61
action_115 (39) = happyShift action_21
action_115 (40) = happyShift action_62
action_115 (41) = happyShift action_22
action_115 (42) = happyShift action_23
action_115 (44) = happyShift action_24
action_115 (47) = happyShift action_25
action_115 (48) = happyShift action_26
action_115 (49) = happyShift action_27
action_115 (50) = happyShift action_28
action_115 (51) = happyShift action_29
action_115 (52) = happyShift action_30
action_115 (53) = happyShift action_31
action_115 (61) = happyShift action_32
action_115 (62) = happyShift action_33
action_115 (63) = happyShift action_34
action_115 (64) = happyShift action_35
action_115 (10) = happyGoto action_47
action_115 (11) = happyGoto action_16
action_115 (13) = happyGoto action_48
action_115 _ = happyReduce_19

happyReduce_3 = happyReduce 6 6 happyReduction_3
happyReduction_3 ((HappyAbsSyn6  happy_var_6) `HappyStk`
	(HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyTerminal (TokenIdLC   p happy_var_2)) `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (addFunc (makeFunc happy_var_1 happy_var_2 happy_var_3 happy_var_5) happy_var_6
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 5 6 happyReduction_4
happyReduction_4 ((HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	(HappyTerminal (TokenIdLC   p happy_var_2)) `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (newProg (makeFunc happy_var_1 happy_var_2 happy_var_3 happy_var_5)
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_2  7 happyReduction_5
happyReduction_5 _
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn7
		 (Just happy_var_1
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_0  7 happyReduction_6
happyReduction_6  =  HappyAbsSyn7
		 (Nothing
	)

happyReduce_7 = happySpecReduce_2  8 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_2)
	(HappyTerminal (TokenIdLC   p happy_var_1))
	 =  HappyAbsSyn8
		 (happy_var_1 : happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_0  8 happyReduction_8
happyReduction_8  =  HappyAbsSyn8
		 ([]
	)

happyReduce_9 = happySpecReduce_1  9 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn9
		 (TInt
	)

happyReduce_10 = happySpecReduce_1  9 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn9
		 (TBool
	)

happyReduce_11 = happySpecReduce_1  9 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn9
		 (TChar
	)

happyReduce_12 = happySpecReduce_1  9 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn9
		 (TList TChar
	)

happyReduce_13 = happySpecReduce_3  9 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (TList happy_var_2
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 5 9 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (TSum happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 5 9 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (TProd happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_3  9 happyReduction_16
happyReduction_16 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (TFunc happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  9 happyReduction_17
happyReduction_17 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  9 happyReduction_18
happyReduction_18 (HappyTerminal (TokenIdLC   p happy_var_1))
	 =  HappyAbsSyn9
		 (ParserTVar happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happyReduce 6 10 happyReduction_19
happyReduction_19 ((HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdLC   p happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (App (AbsInf happy_var_2 happy_var_6) happy_var_4
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 4 10 happyReduction_20
happyReduction_20 ((HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdLC   p happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (AbsInf happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_2  10 happyReduction_21
happyReduction_21 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (App happy_var_1 happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  10 happyReduction_22
happyReduction_22 (HappyTerminal (TokenIdLC   p happy_var_1))
	 =  HappyAbsSyn10
		 (Var happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happyReduce 6 10 happyReduction_23
happyReduction_23 ((HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (App (App (App (Operation Cond) happy_var_2) happy_var_4) happy_var_6
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_3  10 happyReduction_24
happyReduction_24 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (App (App happy_var_2 happy_var_1) happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  10 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn10
		 (Operation IsZ
	)

happyReduce_26 = happySpecReduce_1  10 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn10
		 (Operation Not
	)

happyReduce_27 = happySpecReduce_1  10 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn10
		 (Operation RemL
	)

happyReduce_28 = happySpecReduce_1  10 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn10
		 (Operation RemR
	)

happyReduce_29 = happySpecReduce_1  10 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn10
		 (Operation Fst
	)

happyReduce_30 = happySpecReduce_1  10 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn10
		 (Operation Snd
	)

happyReduce_31 = happySpecReduce_1  10 happyReduction_31
happyReduction_31 _
	 =  HappyAbsSyn10
		 (Operation Head
	)

happyReduce_32 = happySpecReduce_1  10 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn10
		 (Operation Tail
	)

happyReduce_33 = happySpecReduce_1  10 happyReduction_33
happyReduction_33 _
	 =  HappyAbsSyn10
		 (Operation Null
	)

happyReduce_34 = happySpecReduce_1  10 happyReduction_34
happyReduction_34 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  10 happyReduction_35
happyReduction_35 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happyReduce 5 10 happyReduction_36
happyReduction_36 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (App (Operation InjL) happy_var_2
	) `HappyStk` happyRest

happyReduce_37 = happyReduce 5 10 happyReduction_37
happyReduction_37 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (App (Operation InjR) happy_var_4
	) `HappyStk` happyRest

happyReduce_38 = happyReduce 5 10 happyReduction_38
happyReduction_38 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (App (App (Operation Tuple) happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_39 = happySpecReduce_1  10 happyReduction_39
happyReduction_39 (HappyTerminal (TokenInt    p happy_var_1))
	 =  HappyAbsSyn10
		 (Constant (IntVal happy_var_1)
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  10 happyReduction_40
happyReduction_40 (HappyTerminal (TokenBool   p happy_var_1))
	 =  HappyAbsSyn10
		 (Constant (BoolVal happy_var_1)
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  10 happyReduction_41
happyReduction_41 (HappyTerminal (TokenStr    p happy_var_1))
	 =  HappyAbsSyn10
		 (parseStr happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_2  11 happyReduction_42
happyReduction_42 _
	_
	 =  HappyAbsSyn10
		 (Operation Empty
	)

happyReduce_43 = happySpecReduce_3  11 happyReduction_43
happyReduction_43 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (App (App (Operation Cons) happy_var_2) happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  12 happyReduction_44
happyReduction_44 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (App (App (Operation Cons) happy_var_2) happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  12 happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn10
		 (Operation Empty
	)

happyReduce_46 = happySpecReduce_1  13 happyReduction_46
happyReduction_46 _
	 =  HappyAbsSyn10
		 (Operation Add
	)

happyReduce_47 = happySpecReduce_1  13 happyReduction_47
happyReduction_47 _
	 =  HappyAbsSyn10
		 (Operation Sub
	)

happyReduce_48 = happySpecReduce_1  13 happyReduction_48
happyReduction_48 _
	 =  HappyAbsSyn10
		 (Operation Mul
	)

happyReduce_49 = happySpecReduce_1  13 happyReduction_49
happyReduction_49 _
	 =  HappyAbsSyn10
		 (Operation Div
	)

happyReduce_50 = happySpecReduce_1  13 happyReduction_50
happyReduction_50 _
	 =  HappyAbsSyn10
		 (Operation Mod
	)

happyReduce_51 = happySpecReduce_1  13 happyReduction_51
happyReduction_51 _
	 =  HappyAbsSyn10
		 (Operation And
	)

happyReduce_52 = happySpecReduce_1  13 happyReduction_52
happyReduction_52 _
	 =  HappyAbsSyn10
		 (Operation Or
	)

happyReduce_53 = happySpecReduce_1  13 happyReduction_53
happyReduction_53 _
	 =  HappyAbsSyn10
		 (Operation Xor
	)

happyReduce_54 = happySpecReduce_1  13 happyReduction_54
happyReduction_54 _
	 =  HappyAbsSyn10
		 (Operation Lss
	)

happyReduce_55 = happySpecReduce_1  13 happyReduction_55
happyReduction_55 _
	 =  HappyAbsSyn10
		 (Operation LsE
	)

happyReduce_56 = happySpecReduce_1  13 happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn10
		 (Operation Equ
	)

happyReduce_57 = happySpecReduce_1  13 happyReduction_57
happyReduction_57 _
	 =  HappyAbsSyn10
		 (Operation NEq
	)

happyReduce_58 = happySpecReduce_1  13 happyReduction_58
happyReduction_58 _
	 =  HappyAbsSyn10
		 (Operation Gtr
	)

happyReduce_59 = happySpecReduce_1  13 happyReduction_59
happyReduction_59 _
	 =  HappyAbsSyn10
		 (Operation GtE
	)

happyNewToken action sts stk [] =
	action 66 66 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenOpenBr p -> cont 14;
	TokenClosBr p -> cont 15;
	TokenOpenSq p -> cont 16;
	TokenClosSq p -> cont 17;
	TokenOpenCr p -> cont 18;
	TokenClosCr p -> cont 19;
	TokenEquals p -> cont 20;
	TokenComma  p -> cont 21;
	TokenColon  p -> cont 22;
	TokenUndrSc p -> cont 23;
	TokenLambda p -> cont 24;
	TokenDot    p -> cont 25;
	TokenAdd    p -> cont 26;
	TokenSub    p -> cont 27;
	TokenMul    p -> cont 28;
	TokenDiv    p -> cont 29;
	TokenMod    p -> cont 30;
	TokenEqEq   p -> cont 31;
	TokenLsEq   p -> cont 32;
	TokenLess   p -> cont 33;
	TokenGtEq   p -> cont 34;
	TokenGrtr   p -> cont 35;
	TokenNoEq   p -> cont 36;
	TokenAnd    p -> cont 37;
	TokenOr     p -> cont 38;
	TokenNot    p -> cont 39;
	TokenXor    p -> cont 40;
	TokenIsZero p -> cont 41;
	TokenLet    p -> cont 42;
	TokenIn     p -> cont 43;
	TokenIf     p -> cont 44;
	TokenThen   p -> cont 45;
	TokenElse   p -> cont 46;
	TokenRemL   p -> cont 47;
	TokenRemR   p -> cont 48;
	TokenFst    p -> cont 49;
	TokenSnd    p -> cont 50;
	TokenHead   p -> cont 51;
	TokenTail   p -> cont 52;
	TokenNull   p -> cont 53;
	TokenTyInt  p -> cont 54;
	TokenTyBool p -> cont 55;
	TokenTyChar p -> cont 56;
	TokenTyStr  p -> cont 57;
	TokenTySum  p -> cont 58;
	TokenTyProd p -> cont 59;
	TokenTyArrw p -> cont 60;
	TokenInt    p happy_dollar_dollar -> cont 61;
	TokenBool   p happy_dollar_dollar -> cont 62;
	TokenStr    p happy_dollar_dollar -> cont 63;
	TokenIdLC   p happy_dollar_dollar -> cont 64;
	TokenIdUC   p happy_dollar_dollar -> cont 65;
	_ -> happyError' (tk:tks)
	}

happyError_ 66 tk tks = happyError' tks
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
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn6 z -> happyReturn z; _other -> notHappyAtAll })

parseExp tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn10 z -> happyReturn z; _other -> notHappyAtAll })

parseType tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn9 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseStr :: String -> Term
parseStr []     = Operation Empty
parseStr (c:cs) =
    App (App (Operation Cons) (Constant (CharVal c))) (parseStr cs)

parseError :: [Token] -> a
parseError []     = error "Reached end of file while parsing"
parseError (t:ts) = error ("Parse error on line " ++ show (getY t) ++
                           ", column " ++ show (getX t) ++ ".")
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}





# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4










































{-# LINE 5 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

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

{-# LINE 155 "templates/GenericTemplate.hs" #-}

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
     let i = (case x of { HappyErrorToken (i) -> i }) in
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
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
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
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
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
