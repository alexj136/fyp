{-# OPTIONS_GHC -w #-}
module Parser where

import Lexer
import Syntax
import PostParsing

{-- CONTEXT FREE GRAMMAR

PROG ::= type ID = TY
       | def ID ARGS = EXP
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
STR  ::= ".*"

--}

-- parser produced by Happy Version 1.19.3

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn6 (ParseResult)
	| HappyAbsSyn7 ([String])
	| HappyAbsSyn8 (Type)
	| HappyAbsSyn9 (Term)
	| HappyAbsSyn13 (String)

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
 action_113 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
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
 happyReduce_59,
 happyReduce_60,
 happyReduce_61 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (23) = happyShift action_38
action_0 (24) = happyShift action_4
action_0 (6) = happyGoto action_37
action_0 _ = happyReduce_5

action_1 (14) = happyShift action_18
action_1 (16) = happyShift action_19
action_1 (18) = happyShift action_20
action_1 (25) = happyShift action_21
action_1 (40) = happyShift action_22
action_1 (42) = happyShift action_23
action_1 (43) = happyShift action_24
action_1 (45) = happyShift action_25
action_1 (48) = happyShift action_26
action_1 (49) = happyShift action_27
action_1 (50) = happyShift action_28
action_1 (51) = happyShift action_29
action_1 (52) = happyShift action_30
action_1 (53) = happyShift action_31
action_1 (54) = happyShift action_32
action_1 (63) = happyShift action_33
action_1 (64) = happyShift action_34
action_1 (65) = happyShift action_35
action_1 (66) = happyShift action_36
action_1 (9) = happyGoto action_16
action_1 (10) = happyGoto action_17
action_1 _ = happyFail

action_2 (14) = happyShift action_7
action_2 (16) = happyShift action_8
action_2 (18) = happyShift action_9
action_2 (56) = happyShift action_10
action_2 (57) = happyShift action_11
action_2 (58) = happyShift action_12
action_2 (59) = happyShift action_13
action_2 (66) = happyShift action_14
action_2 (67) = happyShift action_15
action_2 (8) = happyGoto action_5
action_2 (13) = happyGoto action_6
action_2 _ = happyFail

action_3 (24) = happyShift action_4
action_3 _ = happyFail

action_4 (66) = happyShift action_69
action_4 _ = happyFail

action_5 (62) = happyShift action_68
action_5 (68) = happyAccept
action_5 _ = happyFail

action_6 _ = happyReduce_17

action_7 (14) = happyShift action_7
action_7 (16) = happyShift action_8
action_7 (18) = happyShift action_9
action_7 (56) = happyShift action_10
action_7 (57) = happyShift action_11
action_7 (58) = happyShift action_12
action_7 (59) = happyShift action_13
action_7 (66) = happyShift action_14
action_7 (67) = happyShift action_15
action_7 (8) = happyGoto action_67
action_7 (13) = happyGoto action_6
action_7 _ = happyFail

action_8 (14) = happyShift action_7
action_8 (16) = happyShift action_8
action_8 (18) = happyShift action_9
action_8 (56) = happyShift action_10
action_8 (57) = happyShift action_11
action_8 (58) = happyShift action_12
action_8 (59) = happyShift action_13
action_8 (66) = happyShift action_14
action_8 (67) = happyShift action_15
action_8 (8) = happyGoto action_66
action_8 (13) = happyGoto action_6
action_8 _ = happyFail

action_9 (14) = happyShift action_7
action_9 (16) = happyShift action_8
action_9 (18) = happyShift action_9
action_9 (56) = happyShift action_10
action_9 (57) = happyShift action_11
action_9 (58) = happyShift action_12
action_9 (59) = happyShift action_13
action_9 (66) = happyShift action_14
action_9 (67) = happyShift action_15
action_9 (8) = happyGoto action_65
action_9 (13) = happyGoto action_6
action_9 _ = happyFail

action_10 _ = happyReduce_8

action_11 _ = happyReduce_9

action_12 _ = happyReduce_10

action_13 _ = happyReduce_11

action_14 _ = happyReduce_60

action_15 _ = happyReduce_61

action_16 (14) = happyShift action_18
action_16 (16) = happyShift action_19
action_16 (18) = happyShift action_20
action_16 (25) = happyShift action_21
action_16 (27) = happyShift action_50
action_16 (28) = happyShift action_51
action_16 (29) = happyShift action_52
action_16 (30) = happyShift action_53
action_16 (31) = happyShift action_54
action_16 (32) = happyShift action_55
action_16 (33) = happyShift action_56
action_16 (34) = happyShift action_57
action_16 (35) = happyShift action_58
action_16 (36) = happyShift action_59
action_16 (37) = happyShift action_60
action_16 (38) = happyShift action_61
action_16 (39) = happyShift action_62
action_16 (40) = happyShift action_22
action_16 (41) = happyShift action_63
action_16 (42) = happyShift action_23
action_16 (43) = happyShift action_24
action_16 (45) = happyShift action_25
action_16 (48) = happyShift action_26
action_16 (49) = happyShift action_27
action_16 (50) = happyShift action_28
action_16 (51) = happyShift action_29
action_16 (52) = happyShift action_30
action_16 (53) = happyShift action_31
action_16 (54) = happyShift action_32
action_16 (55) = happyShift action_64
action_16 (63) = happyShift action_33
action_16 (64) = happyShift action_34
action_16 (65) = happyShift action_35
action_16 (66) = happyShift action_36
action_16 (68) = happyAccept
action_16 (9) = happyGoto action_48
action_16 (10) = happyGoto action_17
action_16 (12) = happyGoto action_49
action_16 _ = happyFail

action_17 _ = happyReduce_33

action_18 (14) = happyShift action_18
action_18 (16) = happyShift action_19
action_18 (18) = happyShift action_20
action_18 (25) = happyShift action_21
action_18 (40) = happyShift action_22
action_18 (42) = happyShift action_23
action_18 (43) = happyShift action_24
action_18 (45) = happyShift action_25
action_18 (48) = happyShift action_26
action_18 (49) = happyShift action_27
action_18 (50) = happyShift action_28
action_18 (51) = happyShift action_29
action_18 (52) = happyShift action_30
action_18 (53) = happyShift action_31
action_18 (54) = happyShift action_32
action_18 (63) = happyShift action_33
action_18 (64) = happyShift action_34
action_18 (65) = happyShift action_35
action_18 (66) = happyShift action_36
action_18 (9) = happyGoto action_47
action_18 (10) = happyGoto action_17
action_18 _ = happyFail

action_19 (14) = happyShift action_18
action_19 (16) = happyShift action_19
action_19 (17) = happyShift action_46
action_19 (18) = happyShift action_20
action_19 (25) = happyShift action_21
action_19 (40) = happyShift action_22
action_19 (42) = happyShift action_23
action_19 (43) = happyShift action_24
action_19 (45) = happyShift action_25
action_19 (48) = happyShift action_26
action_19 (49) = happyShift action_27
action_19 (50) = happyShift action_28
action_19 (51) = happyShift action_29
action_19 (52) = happyShift action_30
action_19 (53) = happyShift action_31
action_19 (54) = happyShift action_32
action_19 (63) = happyShift action_33
action_19 (64) = happyShift action_34
action_19 (65) = happyShift action_35
action_19 (66) = happyShift action_36
action_19 (9) = happyGoto action_45
action_19 (10) = happyGoto action_17
action_19 _ = happyFail

action_20 (14) = happyShift action_18
action_20 (16) = happyShift action_19
action_20 (18) = happyShift action_20
action_20 (22) = happyShift action_44
action_20 (25) = happyShift action_21
action_20 (40) = happyShift action_22
action_20 (42) = happyShift action_23
action_20 (43) = happyShift action_24
action_20 (45) = happyShift action_25
action_20 (48) = happyShift action_26
action_20 (49) = happyShift action_27
action_20 (50) = happyShift action_28
action_20 (51) = happyShift action_29
action_20 (52) = happyShift action_30
action_20 (53) = happyShift action_31
action_20 (54) = happyShift action_32
action_20 (63) = happyShift action_33
action_20 (64) = happyShift action_34
action_20 (65) = happyShift action_35
action_20 (66) = happyShift action_36
action_20 (9) = happyGoto action_43
action_20 (10) = happyGoto action_17
action_20 _ = happyFail

action_21 (66) = happyShift action_42
action_21 _ = happyFail

action_22 _ = happyReduce_25

action_23 _ = happyReduce_24

action_24 (66) = happyShift action_41
action_24 _ = happyFail

action_25 (14) = happyShift action_18
action_25 (16) = happyShift action_19
action_25 (18) = happyShift action_20
action_25 (25) = happyShift action_21
action_25 (40) = happyShift action_22
action_25 (42) = happyShift action_23
action_25 (43) = happyShift action_24
action_25 (45) = happyShift action_25
action_25 (48) = happyShift action_26
action_25 (49) = happyShift action_27
action_25 (50) = happyShift action_28
action_25 (51) = happyShift action_29
action_25 (52) = happyShift action_30
action_25 (53) = happyShift action_31
action_25 (54) = happyShift action_32
action_25 (63) = happyShift action_33
action_25 (64) = happyShift action_34
action_25 (65) = happyShift action_35
action_25 (66) = happyShift action_36
action_25 (9) = happyGoto action_40
action_25 (10) = happyGoto action_17
action_25 _ = happyFail

action_26 _ = happyReduce_26

action_27 _ = happyReduce_27

action_28 _ = happyReduce_28

action_29 _ = happyReduce_29

action_30 _ = happyReduce_30

action_31 _ = happyReduce_31

action_32 _ = happyReduce_32

action_33 _ = happyReduce_38

action_34 _ = happyReduce_39

action_35 _ = happyReduce_40

action_36 _ = happyReduce_21

action_37 (68) = happyAccept
action_37 _ = happyFail

action_38 (66) = happyShift action_39
action_38 _ = happyFail

action_39 (66) = happyShift action_85
action_39 (7) = happyGoto action_87
action_39 _ = happyReduce_7

action_40 (14) = happyShift action_18
action_40 (16) = happyShift action_19
action_40 (18) = happyShift action_20
action_40 (25) = happyShift action_21
action_40 (27) = happyShift action_50
action_40 (28) = happyShift action_51
action_40 (29) = happyShift action_52
action_40 (30) = happyShift action_53
action_40 (31) = happyShift action_54
action_40 (32) = happyShift action_55
action_40 (33) = happyShift action_56
action_40 (34) = happyShift action_57
action_40 (35) = happyShift action_58
action_40 (36) = happyShift action_59
action_40 (37) = happyShift action_60
action_40 (38) = happyShift action_61
action_40 (39) = happyShift action_62
action_40 (40) = happyShift action_22
action_40 (41) = happyShift action_63
action_40 (42) = happyShift action_23
action_40 (43) = happyShift action_24
action_40 (45) = happyShift action_25
action_40 (46) = happyShift action_86
action_40 (48) = happyShift action_26
action_40 (49) = happyShift action_27
action_40 (50) = happyShift action_28
action_40 (51) = happyShift action_29
action_40 (52) = happyShift action_30
action_40 (53) = happyShift action_31
action_40 (54) = happyShift action_32
action_40 (55) = happyShift action_64
action_40 (63) = happyShift action_33
action_40 (64) = happyShift action_34
action_40 (65) = happyShift action_35
action_40 (66) = happyShift action_36
action_40 (9) = happyGoto action_48
action_40 (10) = happyGoto action_17
action_40 (12) = happyGoto action_49
action_40 _ = happyFail

action_41 (66) = happyShift action_85
action_41 (7) = happyGoto action_84
action_41 _ = happyReduce_7

action_42 (26) = happyShift action_83
action_42 _ = happyFail

action_43 (14) = happyShift action_18
action_43 (16) = happyShift action_19
action_43 (18) = happyShift action_20
action_43 (21) = happyShift action_82
action_43 (25) = happyShift action_21
action_43 (27) = happyShift action_50
action_43 (28) = happyShift action_51
action_43 (29) = happyShift action_52
action_43 (30) = happyShift action_53
action_43 (31) = happyShift action_54
action_43 (32) = happyShift action_55
action_43 (33) = happyShift action_56
action_43 (34) = happyShift action_57
action_43 (35) = happyShift action_58
action_43 (36) = happyShift action_59
action_43 (37) = happyShift action_60
action_43 (38) = happyShift action_61
action_43 (39) = happyShift action_62
action_43 (40) = happyShift action_22
action_43 (41) = happyShift action_63
action_43 (42) = happyShift action_23
action_43 (43) = happyShift action_24
action_43 (45) = happyShift action_25
action_43 (48) = happyShift action_26
action_43 (49) = happyShift action_27
action_43 (50) = happyShift action_28
action_43 (51) = happyShift action_29
action_43 (52) = happyShift action_30
action_43 (53) = happyShift action_31
action_43 (54) = happyShift action_32
action_43 (55) = happyShift action_64
action_43 (63) = happyShift action_33
action_43 (64) = happyShift action_34
action_43 (65) = happyShift action_35
action_43 (66) = happyShift action_36
action_43 (9) = happyGoto action_48
action_43 (10) = happyGoto action_17
action_43 (12) = happyGoto action_49
action_43 _ = happyFail

action_44 (21) = happyShift action_81
action_44 _ = happyFail

action_45 (14) = happyShift action_18
action_45 (16) = happyShift action_19
action_45 (17) = happyShift action_79
action_45 (18) = happyShift action_20
action_45 (21) = happyShift action_80
action_45 (25) = happyShift action_21
action_45 (27) = happyShift action_50
action_45 (28) = happyShift action_51
action_45 (29) = happyShift action_52
action_45 (30) = happyShift action_53
action_45 (31) = happyShift action_54
action_45 (32) = happyShift action_55
action_45 (33) = happyShift action_56
action_45 (34) = happyShift action_57
action_45 (35) = happyShift action_58
action_45 (36) = happyShift action_59
action_45 (37) = happyShift action_60
action_45 (38) = happyShift action_61
action_45 (39) = happyShift action_62
action_45 (40) = happyShift action_22
action_45 (41) = happyShift action_63
action_45 (42) = happyShift action_23
action_45 (43) = happyShift action_24
action_45 (45) = happyShift action_25
action_45 (48) = happyShift action_26
action_45 (49) = happyShift action_27
action_45 (50) = happyShift action_28
action_45 (51) = happyShift action_29
action_45 (52) = happyShift action_30
action_45 (53) = happyShift action_31
action_45 (54) = happyShift action_32
action_45 (55) = happyShift action_64
action_45 (63) = happyShift action_33
action_45 (64) = happyShift action_34
action_45 (65) = happyShift action_35
action_45 (66) = happyShift action_36
action_45 (9) = happyGoto action_48
action_45 (10) = happyGoto action_17
action_45 (11) = happyGoto action_78
action_45 (12) = happyGoto action_49
action_45 _ = happyFail

action_46 _ = happyReduce_41

action_47 (14) = happyShift action_18
action_47 (15) = happyShift action_77
action_47 (16) = happyShift action_19
action_47 (18) = happyShift action_20
action_47 (25) = happyShift action_21
action_47 (27) = happyShift action_50
action_47 (28) = happyShift action_51
action_47 (29) = happyShift action_52
action_47 (30) = happyShift action_53
action_47 (31) = happyShift action_54
action_47 (32) = happyShift action_55
action_47 (33) = happyShift action_56
action_47 (34) = happyShift action_57
action_47 (35) = happyShift action_58
action_47 (36) = happyShift action_59
action_47 (37) = happyShift action_60
action_47 (38) = happyShift action_61
action_47 (39) = happyShift action_62
action_47 (40) = happyShift action_22
action_47 (41) = happyShift action_63
action_47 (42) = happyShift action_23
action_47 (43) = happyShift action_24
action_47 (45) = happyShift action_25
action_47 (48) = happyShift action_26
action_47 (49) = happyShift action_27
action_47 (50) = happyShift action_28
action_47 (51) = happyShift action_29
action_47 (52) = happyShift action_30
action_47 (53) = happyShift action_31
action_47 (54) = happyShift action_32
action_47 (55) = happyShift action_64
action_47 (63) = happyShift action_33
action_47 (64) = happyShift action_34
action_47 (65) = happyShift action_35
action_47 (66) = happyShift action_36
action_47 (9) = happyGoto action_48
action_47 (10) = happyGoto action_17
action_47 (12) = happyGoto action_49
action_47 _ = happyFail

action_48 (25) = happyShift action_21
action_48 (9) = happyGoto action_48
action_48 (10) = happyGoto action_17
action_48 (12) = happyGoto action_49
action_48 _ = happyReduce_20

action_49 (14) = happyShift action_18
action_49 (16) = happyShift action_19
action_49 (18) = happyShift action_20
action_49 (25) = happyShift action_21
action_49 (40) = happyShift action_22
action_49 (42) = happyShift action_23
action_49 (43) = happyShift action_24
action_49 (45) = happyShift action_25
action_49 (48) = happyShift action_26
action_49 (49) = happyShift action_27
action_49 (50) = happyShift action_28
action_49 (51) = happyShift action_29
action_49 (52) = happyShift action_30
action_49 (53) = happyShift action_31
action_49 (54) = happyShift action_32
action_49 (63) = happyShift action_33
action_49 (64) = happyShift action_34
action_49 (65) = happyShift action_35
action_49 (66) = happyShift action_36
action_49 (9) = happyGoto action_76
action_49 (10) = happyGoto action_17
action_49 _ = happyFail

action_50 _ = happyReduce_45

action_51 _ = happyReduce_46

action_52 _ = happyReduce_47

action_53 _ = happyReduce_48

action_54 _ = happyReduce_49

action_55 _ = happyReduce_55

action_56 _ = happyReduce_54

action_57 _ = happyReduce_53

action_58 _ = happyReduce_58

action_59 _ = happyReduce_57

action_60 _ = happyReduce_56

action_61 _ = happyReduce_50

action_62 _ = happyReduce_51

action_63 _ = happyReduce_52

action_64 _ = happyReduce_59

action_65 (60) = happyShift action_74
action_65 (61) = happyShift action_75
action_65 (62) = happyShift action_68
action_65 _ = happyFail

action_66 (17) = happyShift action_73
action_66 (62) = happyShift action_68
action_66 _ = happyFail

action_67 (15) = happyShift action_72
action_67 (62) = happyShift action_68
action_67 _ = happyFail

action_68 (14) = happyShift action_7
action_68 (16) = happyShift action_8
action_68 (18) = happyShift action_9
action_68 (56) = happyShift action_10
action_68 (57) = happyShift action_11
action_68 (58) = happyShift action_12
action_68 (59) = happyShift action_13
action_68 (66) = happyShift action_14
action_68 (67) = happyShift action_15
action_68 (8) = happyGoto action_71
action_68 (13) = happyGoto action_6
action_68 _ = happyFail

action_69 (20) = happyShift action_70
action_69 _ = happyFail

action_70 (14) = happyShift action_7
action_70 (16) = happyShift action_8
action_70 (18) = happyShift action_9
action_70 (56) = happyShift action_10
action_70 (57) = happyShift action_11
action_70 (58) = happyShift action_12
action_70 (59) = happyShift action_13
action_70 (66) = happyShift action_14
action_70 (67) = happyShift action_15
action_70 (8) = happyGoto action_99
action_70 (13) = happyGoto action_6
action_70 _ = happyFail

action_71 (62) = happyShift action_68
action_71 _ = happyReduce_15

action_72 _ = happyReduce_16

action_73 _ = happyReduce_12

action_74 (14) = happyShift action_7
action_74 (16) = happyShift action_8
action_74 (18) = happyShift action_9
action_74 (56) = happyShift action_10
action_74 (57) = happyShift action_11
action_74 (58) = happyShift action_12
action_74 (59) = happyShift action_13
action_74 (66) = happyShift action_14
action_74 (67) = happyShift action_15
action_74 (8) = happyGoto action_98
action_74 (13) = happyGoto action_6
action_74 _ = happyFail

action_75 (14) = happyShift action_7
action_75 (16) = happyShift action_8
action_75 (18) = happyShift action_9
action_75 (56) = happyShift action_10
action_75 (57) = happyShift action_11
action_75 (58) = happyShift action_12
action_75 (59) = happyShift action_13
action_75 (66) = happyShift action_14
action_75 (67) = happyShift action_15
action_75 (8) = happyGoto action_97
action_75 (13) = happyGoto action_6
action_75 _ = happyFail

action_76 (14) = happyShift action_18
action_76 (16) = happyShift action_19
action_76 (18) = happyShift action_20
action_76 (25) = happyShift action_21
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
action_76 (39) = happyShift action_62
action_76 (40) = happyShift action_22
action_76 (41) = happyShift action_63
action_76 (42) = happyShift action_23
action_76 (43) = happyShift action_24
action_76 (45) = happyShift action_25
action_76 (48) = happyShift action_26
action_76 (49) = happyShift action_27
action_76 (50) = happyShift action_28
action_76 (51) = happyShift action_29
action_76 (52) = happyShift action_30
action_76 (53) = happyShift action_31
action_76 (54) = happyShift action_32
action_76 (55) = happyShift action_64
action_76 (63) = happyShift action_33
action_76 (64) = happyShift action_34
action_76 (65) = happyShift action_35
action_76 (66) = happyShift action_36
action_76 (9) = happyGoto action_48
action_76 (10) = happyGoto action_17
action_76 (12) = happyGoto action_49
action_76 _ = happyReduce_23

action_77 _ = happyReduce_34

action_78 _ = happyReduce_42

action_79 _ = happyReduce_44

action_80 (14) = happyShift action_18
action_80 (16) = happyShift action_19
action_80 (18) = happyShift action_20
action_80 (25) = happyShift action_21
action_80 (40) = happyShift action_22
action_80 (42) = happyShift action_23
action_80 (43) = happyShift action_24
action_80 (45) = happyShift action_25
action_80 (48) = happyShift action_26
action_80 (49) = happyShift action_27
action_80 (50) = happyShift action_28
action_80 (51) = happyShift action_29
action_80 (52) = happyShift action_30
action_80 (53) = happyShift action_31
action_80 (54) = happyShift action_32
action_80 (63) = happyShift action_33
action_80 (64) = happyShift action_34
action_80 (65) = happyShift action_35
action_80 (66) = happyShift action_36
action_80 (9) = happyGoto action_96
action_80 (10) = happyGoto action_17
action_80 _ = happyFail

action_81 (14) = happyShift action_18
action_81 (16) = happyShift action_19
action_81 (18) = happyShift action_20
action_81 (25) = happyShift action_21
action_81 (40) = happyShift action_22
action_81 (42) = happyShift action_23
action_81 (43) = happyShift action_24
action_81 (45) = happyShift action_25
action_81 (48) = happyShift action_26
action_81 (49) = happyShift action_27
action_81 (50) = happyShift action_28
action_81 (51) = happyShift action_29
action_81 (52) = happyShift action_30
action_81 (53) = happyShift action_31
action_81 (54) = happyShift action_32
action_81 (63) = happyShift action_33
action_81 (64) = happyShift action_34
action_81 (65) = happyShift action_35
action_81 (66) = happyShift action_36
action_81 (9) = happyGoto action_95
action_81 (10) = happyGoto action_17
action_81 _ = happyFail

action_82 (14) = happyShift action_18
action_82 (16) = happyShift action_19
action_82 (18) = happyShift action_20
action_82 (22) = happyShift action_94
action_82 (25) = happyShift action_21
action_82 (40) = happyShift action_22
action_82 (42) = happyShift action_23
action_82 (43) = happyShift action_24
action_82 (45) = happyShift action_25
action_82 (48) = happyShift action_26
action_82 (49) = happyShift action_27
action_82 (50) = happyShift action_28
action_82 (51) = happyShift action_29
action_82 (52) = happyShift action_30
action_82 (53) = happyShift action_31
action_82 (54) = happyShift action_32
action_82 (63) = happyShift action_33
action_82 (64) = happyShift action_34
action_82 (65) = happyShift action_35
action_82 (66) = happyShift action_36
action_82 (9) = happyGoto action_93
action_82 (10) = happyGoto action_17
action_82 _ = happyFail

action_83 (14) = happyShift action_18
action_83 (16) = happyShift action_19
action_83 (18) = happyShift action_20
action_83 (25) = happyShift action_21
action_83 (40) = happyShift action_22
action_83 (42) = happyShift action_23
action_83 (43) = happyShift action_24
action_83 (45) = happyShift action_25
action_83 (48) = happyShift action_26
action_83 (49) = happyShift action_27
action_83 (50) = happyShift action_28
action_83 (51) = happyShift action_29
action_83 (52) = happyShift action_30
action_83 (53) = happyShift action_31
action_83 (54) = happyShift action_32
action_83 (63) = happyShift action_33
action_83 (64) = happyShift action_34
action_83 (65) = happyShift action_35
action_83 (66) = happyShift action_36
action_83 (9) = happyGoto action_92
action_83 (10) = happyGoto action_17
action_83 _ = happyFail

action_84 (20) = happyShift action_91
action_84 _ = happyFail

action_85 (66) = happyShift action_85
action_85 (7) = happyGoto action_90
action_85 _ = happyReduce_7

action_86 (14) = happyShift action_18
action_86 (16) = happyShift action_19
action_86 (18) = happyShift action_20
action_86 (25) = happyShift action_21
action_86 (40) = happyShift action_22
action_86 (42) = happyShift action_23
action_86 (43) = happyShift action_24
action_86 (45) = happyShift action_25
action_86 (48) = happyShift action_26
action_86 (49) = happyShift action_27
action_86 (50) = happyShift action_28
action_86 (51) = happyShift action_29
action_86 (52) = happyShift action_30
action_86 (53) = happyShift action_31
action_86 (54) = happyShift action_32
action_86 (63) = happyShift action_33
action_86 (64) = happyShift action_34
action_86 (65) = happyShift action_35
action_86 (66) = happyShift action_36
action_86 (9) = happyGoto action_89
action_86 (10) = happyGoto action_17
action_86 _ = happyFail

action_87 (20) = happyShift action_88
action_87 _ = happyFail

action_88 (14) = happyShift action_18
action_88 (16) = happyShift action_19
action_88 (18) = happyShift action_20
action_88 (25) = happyShift action_21
action_88 (40) = happyShift action_22
action_88 (42) = happyShift action_23
action_88 (43) = happyShift action_24
action_88 (45) = happyShift action_25
action_88 (48) = happyShift action_26
action_88 (49) = happyShift action_27
action_88 (50) = happyShift action_28
action_88 (51) = happyShift action_29
action_88 (52) = happyShift action_30
action_88 (53) = happyShift action_31
action_88 (54) = happyShift action_32
action_88 (63) = happyShift action_33
action_88 (64) = happyShift action_34
action_88 (65) = happyShift action_35
action_88 (66) = happyShift action_36
action_88 (9) = happyGoto action_109
action_88 (10) = happyGoto action_17
action_88 _ = happyFail

action_89 (14) = happyShift action_18
action_89 (16) = happyShift action_19
action_89 (18) = happyShift action_20
action_89 (25) = happyShift action_21
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
action_89 (39) = happyShift action_62
action_89 (40) = happyShift action_22
action_89 (41) = happyShift action_63
action_89 (42) = happyShift action_23
action_89 (43) = happyShift action_24
action_89 (45) = happyShift action_25
action_89 (47) = happyShift action_108
action_89 (48) = happyShift action_26
action_89 (49) = happyShift action_27
action_89 (50) = happyShift action_28
action_89 (51) = happyShift action_29
action_89 (52) = happyShift action_30
action_89 (53) = happyShift action_31
action_89 (54) = happyShift action_32
action_89 (55) = happyShift action_64
action_89 (63) = happyShift action_33
action_89 (64) = happyShift action_34
action_89 (65) = happyShift action_35
action_89 (66) = happyShift action_36
action_89 (9) = happyGoto action_48
action_89 (10) = happyGoto action_17
action_89 (12) = happyGoto action_49
action_89 _ = happyFail

action_90 _ = happyReduce_6

action_91 (14) = happyShift action_18
action_91 (16) = happyShift action_19
action_91 (18) = happyShift action_20
action_91 (25) = happyShift action_21
action_91 (40) = happyShift action_22
action_91 (42) = happyShift action_23
action_91 (43) = happyShift action_24
action_91 (45) = happyShift action_25
action_91 (48) = happyShift action_26
action_91 (49) = happyShift action_27
action_91 (50) = happyShift action_28
action_91 (51) = happyShift action_29
action_91 (52) = happyShift action_30
action_91 (53) = happyShift action_31
action_91 (54) = happyShift action_32
action_91 (63) = happyShift action_33
action_91 (64) = happyShift action_34
action_91 (65) = happyShift action_35
action_91 (66) = happyShift action_36
action_91 (9) = happyGoto action_107
action_91 (10) = happyGoto action_17
action_91 _ = happyFail

action_92 (25) = happyShift action_21
action_92 (27) = happyShift action_50
action_92 (28) = happyShift action_51
action_92 (29) = happyShift action_52
action_92 (30) = happyShift action_53
action_92 (31) = happyShift action_54
action_92 (32) = happyShift action_55
action_92 (33) = happyShift action_56
action_92 (34) = happyShift action_57
action_92 (35) = happyShift action_58
action_92 (36) = happyShift action_59
action_92 (37) = happyShift action_60
action_92 (38) = happyShift action_61
action_92 (39) = happyShift action_62
action_92 (41) = happyShift action_63
action_92 (43) = happyFail
action_92 (45) = happyFail
action_92 (55) = happyShift action_64
action_92 (63) = happyFail
action_92 (64) = happyFail
action_92 (65) = happyFail
action_92 (66) = happyShift action_36
action_92 (9) = happyGoto action_48
action_92 (10) = happyGoto action_17
action_92 (12) = happyGoto action_49
action_92 _ = happyReduce_19

action_93 (14) = happyShift action_18
action_93 (16) = happyShift action_19
action_93 (18) = happyShift action_20
action_93 (19) = happyShift action_106
action_93 (25) = happyShift action_21
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
action_93 (39) = happyShift action_62
action_93 (40) = happyShift action_22
action_93 (41) = happyShift action_63
action_93 (42) = happyShift action_23
action_93 (43) = happyShift action_24
action_93 (45) = happyShift action_25
action_93 (48) = happyShift action_26
action_93 (49) = happyShift action_27
action_93 (50) = happyShift action_28
action_93 (51) = happyShift action_29
action_93 (52) = happyShift action_30
action_93 (53) = happyShift action_31
action_93 (54) = happyShift action_32
action_93 (55) = happyShift action_64
action_93 (63) = happyShift action_33
action_93 (64) = happyShift action_34
action_93 (65) = happyShift action_35
action_93 (66) = happyShift action_36
action_93 (9) = happyGoto action_48
action_93 (10) = happyGoto action_17
action_93 (12) = happyGoto action_49
action_93 _ = happyFail

action_94 (19) = happyShift action_105
action_94 _ = happyFail

action_95 (14) = happyShift action_18
action_95 (16) = happyShift action_19
action_95 (18) = happyShift action_20
action_95 (19) = happyShift action_104
action_95 (25) = happyShift action_21
action_95 (27) = happyShift action_50
action_95 (28) = happyShift action_51
action_95 (29) = happyShift action_52
action_95 (30) = happyShift action_53
action_95 (31) = happyShift action_54
action_95 (32) = happyShift action_55
action_95 (33) = happyShift action_56
action_95 (34) = happyShift action_57
action_95 (35) = happyShift action_58
action_95 (36) = happyShift action_59
action_95 (37) = happyShift action_60
action_95 (38) = happyShift action_61
action_95 (39) = happyShift action_62
action_95 (40) = happyShift action_22
action_95 (41) = happyShift action_63
action_95 (42) = happyShift action_23
action_95 (43) = happyShift action_24
action_95 (45) = happyShift action_25
action_95 (48) = happyShift action_26
action_95 (49) = happyShift action_27
action_95 (50) = happyShift action_28
action_95 (51) = happyShift action_29
action_95 (52) = happyShift action_30
action_95 (53) = happyShift action_31
action_95 (54) = happyShift action_32
action_95 (55) = happyShift action_64
action_95 (63) = happyShift action_33
action_95 (64) = happyShift action_34
action_95 (65) = happyShift action_35
action_95 (66) = happyShift action_36
action_95 (9) = happyGoto action_48
action_95 (10) = happyGoto action_17
action_95 (12) = happyGoto action_49
action_95 _ = happyFail

action_96 (14) = happyShift action_18
action_96 (16) = happyShift action_19
action_96 (17) = happyShift action_79
action_96 (18) = happyShift action_20
action_96 (21) = happyShift action_80
action_96 (25) = happyShift action_21
action_96 (27) = happyShift action_50
action_96 (28) = happyShift action_51
action_96 (29) = happyShift action_52
action_96 (30) = happyShift action_53
action_96 (31) = happyShift action_54
action_96 (32) = happyShift action_55
action_96 (33) = happyShift action_56
action_96 (34) = happyShift action_57
action_96 (35) = happyShift action_58
action_96 (36) = happyShift action_59
action_96 (37) = happyShift action_60
action_96 (38) = happyShift action_61
action_96 (39) = happyShift action_62
action_96 (40) = happyShift action_22
action_96 (41) = happyShift action_63
action_96 (42) = happyShift action_23
action_96 (43) = happyShift action_24
action_96 (45) = happyShift action_25
action_96 (48) = happyShift action_26
action_96 (49) = happyShift action_27
action_96 (50) = happyShift action_28
action_96 (51) = happyShift action_29
action_96 (52) = happyShift action_30
action_96 (53) = happyShift action_31
action_96 (54) = happyShift action_32
action_96 (55) = happyShift action_64
action_96 (63) = happyShift action_33
action_96 (64) = happyShift action_34
action_96 (65) = happyShift action_35
action_96 (66) = happyShift action_36
action_96 (9) = happyGoto action_48
action_96 (10) = happyGoto action_17
action_96 (11) = happyGoto action_103
action_96 (12) = happyGoto action_49
action_96 _ = happyFail

action_97 (19) = happyShift action_102
action_97 (62) = happyShift action_68
action_97 _ = happyFail

action_98 (19) = happyShift action_101
action_98 (62) = happyShift action_68
action_98 _ = happyFail

action_99 (23) = happyShift action_38
action_99 (24) = happyShift action_4
action_99 (62) = happyShift action_68
action_99 (6) = happyGoto action_100
action_99 _ = happyReduce_5

action_100 _ = happyReduce_3

action_101 _ = happyReduce_13

action_102 _ = happyReduce_14

action_103 _ = happyReduce_43

action_104 _ = happyReduce_36

action_105 _ = happyReduce_35

action_106 _ = happyReduce_37

action_107 (14) = happyShift action_18
action_107 (16) = happyShift action_19
action_107 (18) = happyShift action_20
action_107 (25) = happyShift action_21
action_107 (27) = happyShift action_50
action_107 (28) = happyShift action_51
action_107 (29) = happyShift action_52
action_107 (30) = happyShift action_53
action_107 (31) = happyShift action_54
action_107 (32) = happyShift action_55
action_107 (33) = happyShift action_56
action_107 (34) = happyShift action_57
action_107 (35) = happyShift action_58
action_107 (36) = happyShift action_59
action_107 (37) = happyShift action_60
action_107 (38) = happyShift action_61
action_107 (39) = happyShift action_62
action_107 (40) = happyShift action_22
action_107 (41) = happyShift action_63
action_107 (42) = happyShift action_23
action_107 (43) = happyShift action_24
action_107 (44) = happyShift action_112
action_107 (45) = happyShift action_25
action_107 (48) = happyShift action_26
action_107 (49) = happyShift action_27
action_107 (50) = happyShift action_28
action_107 (51) = happyShift action_29
action_107 (52) = happyShift action_30
action_107 (53) = happyShift action_31
action_107 (54) = happyShift action_32
action_107 (55) = happyShift action_64
action_107 (63) = happyShift action_33
action_107 (64) = happyShift action_34
action_107 (65) = happyShift action_35
action_107 (66) = happyShift action_36
action_107 (9) = happyGoto action_48
action_107 (10) = happyGoto action_17
action_107 (12) = happyGoto action_49
action_107 _ = happyFail

action_108 (14) = happyShift action_18
action_108 (16) = happyShift action_19
action_108 (18) = happyShift action_20
action_108 (25) = happyShift action_21
action_108 (40) = happyShift action_22
action_108 (42) = happyShift action_23
action_108 (43) = happyShift action_24
action_108 (45) = happyShift action_25
action_108 (48) = happyShift action_26
action_108 (49) = happyShift action_27
action_108 (50) = happyShift action_28
action_108 (51) = happyShift action_29
action_108 (52) = happyShift action_30
action_108 (53) = happyShift action_31
action_108 (54) = happyShift action_32
action_108 (63) = happyShift action_33
action_108 (64) = happyShift action_34
action_108 (65) = happyShift action_35
action_108 (66) = happyShift action_36
action_108 (9) = happyGoto action_111
action_108 (10) = happyGoto action_17
action_108 _ = happyFail

action_109 (14) = happyShift action_18
action_109 (16) = happyShift action_19
action_109 (18) = happyShift action_20
action_109 (23) = happyShift action_38
action_109 (24) = happyShift action_4
action_109 (25) = happyShift action_21
action_109 (27) = happyShift action_50
action_109 (28) = happyShift action_51
action_109 (29) = happyShift action_52
action_109 (30) = happyShift action_53
action_109 (31) = happyShift action_54
action_109 (32) = happyShift action_55
action_109 (33) = happyShift action_56
action_109 (34) = happyShift action_57
action_109 (35) = happyShift action_58
action_109 (36) = happyShift action_59
action_109 (37) = happyShift action_60
action_109 (38) = happyShift action_61
action_109 (39) = happyShift action_62
action_109 (40) = happyShift action_22
action_109 (41) = happyShift action_63
action_109 (42) = happyShift action_23
action_109 (43) = happyShift action_24
action_109 (45) = happyShift action_25
action_109 (48) = happyShift action_26
action_109 (49) = happyShift action_27
action_109 (50) = happyShift action_28
action_109 (51) = happyShift action_29
action_109 (52) = happyShift action_30
action_109 (53) = happyShift action_31
action_109 (54) = happyShift action_32
action_109 (55) = happyShift action_64
action_109 (63) = happyShift action_33
action_109 (64) = happyShift action_34
action_109 (65) = happyShift action_35
action_109 (66) = happyShift action_36
action_109 (6) = happyGoto action_110
action_109 (9) = happyGoto action_48
action_109 (10) = happyGoto action_17
action_109 (12) = happyGoto action_49
action_109 _ = happyReduce_5

action_110 _ = happyReduce_4

action_111 (25) = happyShift action_21
action_111 (27) = happyShift action_50
action_111 (28) = happyShift action_51
action_111 (29) = happyShift action_52
action_111 (30) = happyShift action_53
action_111 (31) = happyShift action_54
action_111 (32) = happyShift action_55
action_111 (33) = happyShift action_56
action_111 (34) = happyShift action_57
action_111 (35) = happyShift action_58
action_111 (36) = happyShift action_59
action_111 (37) = happyShift action_60
action_111 (38) = happyShift action_61
action_111 (39) = happyShift action_62
action_111 (41) = happyShift action_63
action_111 (43) = happyFail
action_111 (45) = happyFail
action_111 (55) = happyShift action_64
action_111 (63) = happyFail
action_111 (64) = happyFail
action_111 (65) = happyFail
action_111 (66) = happyShift action_36
action_111 (9) = happyGoto action_48
action_111 (10) = happyGoto action_17
action_111 (12) = happyGoto action_49
action_111 _ = happyReduce_22

action_112 (14) = happyShift action_18
action_112 (16) = happyShift action_19
action_112 (18) = happyShift action_20
action_112 (25) = happyShift action_21
action_112 (40) = happyShift action_22
action_112 (42) = happyShift action_23
action_112 (43) = happyShift action_24
action_112 (45) = happyShift action_25
action_112 (48) = happyShift action_26
action_112 (49) = happyShift action_27
action_112 (50) = happyShift action_28
action_112 (51) = happyShift action_29
action_112 (52) = happyShift action_30
action_112 (53) = happyShift action_31
action_112 (54) = happyShift action_32
action_112 (63) = happyShift action_33
action_112 (64) = happyShift action_34
action_112 (65) = happyShift action_35
action_112 (66) = happyShift action_36
action_112 (9) = happyGoto action_113
action_112 (10) = happyGoto action_17
action_112 _ = happyFail

action_113 (25) = happyShift action_21
action_113 (27) = happyShift action_50
action_113 (28) = happyShift action_51
action_113 (29) = happyShift action_52
action_113 (30) = happyShift action_53
action_113 (31) = happyShift action_54
action_113 (32) = happyShift action_55
action_113 (33) = happyShift action_56
action_113 (34) = happyShift action_57
action_113 (35) = happyShift action_58
action_113 (36) = happyShift action_59
action_113 (37) = happyShift action_60
action_113 (38) = happyShift action_61
action_113 (39) = happyShift action_62
action_113 (41) = happyShift action_63
action_113 (43) = happyFail
action_113 (45) = happyFail
action_113 (55) = happyShift action_64
action_113 (63) = happyFail
action_113 (64) = happyFail
action_113 (65) = happyFail
action_113 (66) = happyShift action_36
action_113 (9) = happyGoto action_48
action_113 (10) = happyGoto action_17
action_113 (12) = happyGoto action_49
action_113 _ = happyReduce_18

happyReduce_3 = happyReduce 5 6 happyReduction_3
happyReduction_3 ((HappyAbsSyn6  happy_var_5) `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdLC   p happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (addTyDec happy_var_5 (happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 6 6 happyReduction_4
happyReduction_4 ((HappyAbsSyn6  happy_var_6) `HappyStk`
	(HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	(HappyTerminal (TokenIdLC   p happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (addFuncPR happy_var_6 (FuncInf happy_var_2 happy_var_3 happy_var_5)
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_0  6 happyReduction_5
happyReduction_5  =  HappyAbsSyn6
		 (emptyPR
	)

happyReduce_6 = happySpecReduce_2  7 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal (TokenIdLC   p happy_var_1))
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  7 happyReduction_7
happyReduction_7  =  HappyAbsSyn7
		 ([]
	)

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn8
		 (TInt
	)

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn8
		 (TBool
	)

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn8
		 (TChar
	)

happyReduce_11 = happySpecReduce_1  8 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn8
		 (TList TChar
	)

happyReduce_12 = happySpecReduce_3  8 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (TList happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 5 8 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TSum happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 5 8 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TProd happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_3  8 happyReduction_15
happyReduction_15 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (TFunc happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  8 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  8 happyReduction_17
happyReduction_17 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn8
		 (ParserTVar happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happyReduce 7 9 happyReduction_18
happyReduction_18 ((HappyAbsSyn9  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	(HappyTerminal (TokenIdLC   p happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (App (AbsInf happy_var_2 happy_var_7) (toLambdas (FuncInf happy_var_2 happy_var_3 happy_var_5))
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 4 9 happyReduction_19
happyReduction_19 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdLC   p happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (AbsInf happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_2  9 happyReduction_20
happyReduction_20 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (App happy_var_1 happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  9 happyReduction_21
happyReduction_21 (HappyTerminal (TokenIdLC   p happy_var_1))
	 =  HappyAbsSyn9
		 (Var happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happyReduce 6 9 happyReduction_22
happyReduction_22 ((HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (App (App (App (Operation Cond) happy_var_2) happy_var_4) happy_var_6
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_3  9 happyReduction_23
happyReduction_23 (HappyAbsSyn9  happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (App (App happy_var_2 happy_var_1) happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  9 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn9
		 (Operation IsZ
	)

happyReduce_25 = happySpecReduce_1  9 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn9
		 (Operation Not
	)

happyReduce_26 = happySpecReduce_1  9 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn9
		 (Operation RemL
	)

happyReduce_27 = happySpecReduce_1  9 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn9
		 (Operation RemR
	)

happyReduce_28 = happySpecReduce_1  9 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn9
		 (Operation Fst
	)

happyReduce_29 = happySpecReduce_1  9 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn9
		 (Operation Snd
	)

happyReduce_30 = happySpecReduce_1  9 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn9
		 (Operation Head
	)

happyReduce_31 = happySpecReduce_1  9 happyReduction_31
happyReduction_31 _
	 =  HappyAbsSyn9
		 (Operation Tail
	)

happyReduce_32 = happySpecReduce_1  9 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn9
		 (Operation Null
	)

happyReduce_33 = happySpecReduce_1  9 happyReduction_33
happyReduction_33 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  9 happyReduction_34
happyReduction_34 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happyReduce 5 9 happyReduction_35
happyReduction_35 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (App (Operation InjL) happy_var_2
	) `HappyStk` happyRest

happyReduce_36 = happyReduce 5 9 happyReduction_36
happyReduction_36 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (App (Operation InjR) happy_var_4
	) `HappyStk` happyRest

happyReduce_37 = happyReduce 5 9 happyReduction_37
happyReduction_37 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (App (App (Operation Tuple) happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_38 = happySpecReduce_1  9 happyReduction_38
happyReduction_38 (HappyTerminal (TokenInt    p happy_var_1))
	 =  HappyAbsSyn9
		 (Constant (IntVal happy_var_1)
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  9 happyReduction_39
happyReduction_39 (HappyTerminal (TokenBool   p happy_var_1))
	 =  HappyAbsSyn9
		 (Constant (BoolVal happy_var_1)
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  9 happyReduction_40
happyReduction_40 (HappyTerminal (TokenStr    p happy_var_1))
	 =  HappyAbsSyn9
		 (parseStr happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_2  10 happyReduction_41
happyReduction_41 _
	_
	 =  HappyAbsSyn9
		 (Operation Empty
	)

happyReduce_42 = happySpecReduce_3  10 happyReduction_42
happyReduction_42 (HappyAbsSyn9  happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (App (App (Operation Cons) happy_var_2) happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  11 happyReduction_43
happyReduction_43 (HappyAbsSyn9  happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (App (App (Operation Cons) happy_var_2) happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  11 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn9
		 (Operation Empty
	)

happyReduce_45 = happySpecReduce_1  12 happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn9
		 (Operation Add
	)

happyReduce_46 = happySpecReduce_1  12 happyReduction_46
happyReduction_46 _
	 =  HappyAbsSyn9
		 (Operation Sub
	)

happyReduce_47 = happySpecReduce_1  12 happyReduction_47
happyReduction_47 _
	 =  HappyAbsSyn9
		 (Operation Mul
	)

happyReduce_48 = happySpecReduce_1  12 happyReduction_48
happyReduction_48 _
	 =  HappyAbsSyn9
		 (Operation Div
	)

happyReduce_49 = happySpecReduce_1  12 happyReduction_49
happyReduction_49 _
	 =  HappyAbsSyn9
		 (Operation Mod
	)

happyReduce_50 = happySpecReduce_1  12 happyReduction_50
happyReduction_50 _
	 =  HappyAbsSyn9
		 (Operation And
	)

happyReduce_51 = happySpecReduce_1  12 happyReduction_51
happyReduction_51 _
	 =  HappyAbsSyn9
		 (Operation Or
	)

happyReduce_52 = happySpecReduce_1  12 happyReduction_52
happyReduction_52 _
	 =  HappyAbsSyn9
		 (Operation Xor
	)

happyReduce_53 = happySpecReduce_1  12 happyReduction_53
happyReduction_53 _
	 =  HappyAbsSyn9
		 (Operation Lss
	)

happyReduce_54 = happySpecReduce_1  12 happyReduction_54
happyReduction_54 _
	 =  HappyAbsSyn9
		 (Operation LsE
	)

happyReduce_55 = happySpecReduce_1  12 happyReduction_55
happyReduction_55 _
	 =  HappyAbsSyn9
		 (Operation Equ
	)

happyReduce_56 = happySpecReduce_1  12 happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn9
		 (Operation NEq
	)

happyReduce_57 = happySpecReduce_1  12 happyReduction_57
happyReduction_57 _
	 =  HappyAbsSyn9
		 (Operation Gtr
	)

happyReduce_58 = happySpecReduce_1  12 happyReduction_58
happyReduction_58 _
	 =  HappyAbsSyn9
		 (Operation GtE
	)

happyReduce_59 = happySpecReduce_1  12 happyReduction_59
happyReduction_59 _
	 =  HappyAbsSyn9
		 (Operation Cons
	)

happyReduce_60 = happySpecReduce_1  13 happyReduction_60
happyReduction_60 (HappyTerminal (TokenIdLC   p happy_var_1))
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  13 happyReduction_61
happyReduction_61 (HappyTerminal (TokenIdUC   p happy_var_1))
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 68 68 notHappyAtAll (HappyState action) sts stk []

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
	TokenUndrSc p -> cont 22;
	TokenDef    p -> cont 23;
	TokenType   p -> cont 24;
	TokenLambda p -> cont 25;
	TokenDot    p -> cont 26;
	TokenAdd    p -> cont 27;
	TokenSub    p -> cont 28;
	TokenMul    p -> cont 29;
	TokenDiv    p -> cont 30;
	TokenMod    p -> cont 31;
	TokenEqEq   p -> cont 32;
	TokenLsEq   p -> cont 33;
	TokenLess   p -> cont 34;
	TokenGtEq   p -> cont 35;
	TokenGrtr   p -> cont 36;
	TokenNoEq   p -> cont 37;
	TokenAnd    p -> cont 38;
	TokenOr     p -> cont 39;
	TokenNot    p -> cont 40;
	TokenXor    p -> cont 41;
	TokenIsZero p -> cont 42;
	TokenLet    p -> cont 43;
	TokenIn     p -> cont 44;
	TokenIf     p -> cont 45;
	TokenThen   p -> cont 46;
	TokenElse   p -> cont 47;
	TokenRemL   p -> cont 48;
	TokenRemR   p -> cont 49;
	TokenFst    p -> cont 50;
	TokenSnd    p -> cont 51;
	TokenHead   p -> cont 52;
	TokenTail   p -> cont 53;
	TokenNull   p -> cont 54;
	TokenCons   p -> cont 55;
	TokenTyInt  p -> cont 56;
	TokenTyBool p -> cont 57;
	TokenTyChar p -> cont 58;
	TokenTyStr  p -> cont 59;
	TokenTySum  p -> cont 60;
	TokenTyProd p -> cont 61;
	TokenTyArrw p -> cont 62;
	TokenInt    p happy_dollar_dollar -> cont 63;
	TokenBool   p happy_dollar_dollar -> cont 64;
	TokenStr    p happy_dollar_dollar -> cont 65;
	TokenIdLC   p happy_dollar_dollar -> cont 66;
	TokenIdUC   p happy_dollar_dollar -> cont 67;
	_ -> happyError' (tk:tks)
	}

happyError_ 68 tk tks = happyError' tks
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
  happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn9 z -> happyReturn z; _other -> notHappyAtAll })

parseType tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn8 z -> happyReturn z; _other -> notHappyAtAll })

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
