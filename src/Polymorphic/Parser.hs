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
 action_113,
 action_114 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
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
 happyReduce_61,
 happyReduce_62 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (23) = happyShift action_39
action_0 (24) = happyShift action_4
action_0 (6) = happyGoto action_38
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
action_1 (55) = happyShift action_33
action_1 (64) = happyShift action_34
action_1 (65) = happyShift action_35
action_1 (66) = happyShift action_36
action_1 (67) = happyShift action_37
action_1 (9) = happyGoto action_16
action_1 (10) = happyGoto action_17
action_1 _ = happyFail

action_2 (14) = happyShift action_7
action_2 (16) = happyShift action_8
action_2 (18) = happyShift action_9
action_2 (57) = happyShift action_10
action_2 (58) = happyShift action_11
action_2 (59) = happyShift action_12
action_2 (60) = happyShift action_13
action_2 (67) = happyShift action_14
action_2 (68) = happyShift action_15
action_2 (8) = happyGoto action_5
action_2 (13) = happyGoto action_6
action_2 _ = happyFail

action_3 (24) = happyShift action_4
action_3 _ = happyFail

action_4 (67) = happyShift action_70
action_4 _ = happyFail

action_5 (63) = happyShift action_69
action_5 (69) = happyAccept
action_5 _ = happyFail

action_6 _ = happyReduce_17

action_7 (14) = happyShift action_7
action_7 (16) = happyShift action_8
action_7 (18) = happyShift action_9
action_7 (57) = happyShift action_10
action_7 (58) = happyShift action_11
action_7 (59) = happyShift action_12
action_7 (60) = happyShift action_13
action_7 (67) = happyShift action_14
action_7 (68) = happyShift action_15
action_7 (8) = happyGoto action_68
action_7 (13) = happyGoto action_6
action_7 _ = happyFail

action_8 (14) = happyShift action_7
action_8 (16) = happyShift action_8
action_8 (18) = happyShift action_9
action_8 (57) = happyShift action_10
action_8 (58) = happyShift action_11
action_8 (59) = happyShift action_12
action_8 (60) = happyShift action_13
action_8 (67) = happyShift action_14
action_8 (68) = happyShift action_15
action_8 (8) = happyGoto action_67
action_8 (13) = happyGoto action_6
action_8 _ = happyFail

action_9 (14) = happyShift action_7
action_9 (16) = happyShift action_8
action_9 (18) = happyShift action_9
action_9 (57) = happyShift action_10
action_9 (58) = happyShift action_11
action_9 (59) = happyShift action_12
action_9 (60) = happyShift action_13
action_9 (67) = happyShift action_14
action_9 (68) = happyShift action_15
action_9 (8) = happyGoto action_66
action_9 (13) = happyGoto action_6
action_9 _ = happyFail

action_10 _ = happyReduce_8

action_11 _ = happyReduce_9

action_12 _ = happyReduce_10

action_13 _ = happyReduce_11

action_14 _ = happyReduce_61

action_15 _ = happyReduce_62

action_16 (14) = happyShift action_18
action_16 (16) = happyShift action_19
action_16 (18) = happyShift action_20
action_16 (25) = happyShift action_21
action_16 (27) = happyShift action_51
action_16 (28) = happyShift action_52
action_16 (29) = happyShift action_53
action_16 (30) = happyShift action_54
action_16 (31) = happyShift action_55
action_16 (32) = happyShift action_56
action_16 (33) = happyShift action_57
action_16 (34) = happyShift action_58
action_16 (35) = happyShift action_59
action_16 (36) = happyShift action_60
action_16 (37) = happyShift action_61
action_16 (38) = happyShift action_62
action_16 (39) = happyShift action_63
action_16 (40) = happyShift action_22
action_16 (41) = happyShift action_64
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
action_16 (55) = happyShift action_33
action_16 (56) = happyShift action_65
action_16 (64) = happyShift action_34
action_16 (65) = happyShift action_35
action_16 (66) = happyShift action_36
action_16 (67) = happyShift action_37
action_16 (69) = happyAccept
action_16 (9) = happyGoto action_49
action_16 (10) = happyGoto action_17
action_16 (12) = happyGoto action_50
action_16 _ = happyFail

action_17 _ = happyReduce_34

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
action_18 (55) = happyShift action_33
action_18 (64) = happyShift action_34
action_18 (65) = happyShift action_35
action_18 (66) = happyShift action_36
action_18 (67) = happyShift action_37
action_18 (9) = happyGoto action_48
action_18 (10) = happyGoto action_17
action_18 _ = happyFail

action_19 (14) = happyShift action_18
action_19 (16) = happyShift action_19
action_19 (17) = happyShift action_47
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
action_19 (55) = happyShift action_33
action_19 (64) = happyShift action_34
action_19 (65) = happyShift action_35
action_19 (66) = happyShift action_36
action_19 (67) = happyShift action_37
action_19 (9) = happyGoto action_46
action_19 (10) = happyGoto action_17
action_19 _ = happyFail

action_20 (14) = happyShift action_18
action_20 (16) = happyShift action_19
action_20 (18) = happyShift action_20
action_20 (22) = happyShift action_45
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
action_20 (55) = happyShift action_33
action_20 (64) = happyShift action_34
action_20 (65) = happyShift action_35
action_20 (66) = happyShift action_36
action_20 (67) = happyShift action_37
action_20 (9) = happyGoto action_44
action_20 (10) = happyGoto action_17
action_20 _ = happyFail

action_21 (67) = happyShift action_43
action_21 _ = happyFail

action_22 _ = happyReduce_25

action_23 _ = happyReduce_24

action_24 (67) = happyShift action_42
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
action_25 (55) = happyShift action_33
action_25 (64) = happyShift action_34
action_25 (65) = happyShift action_35
action_25 (66) = happyShift action_36
action_25 (67) = happyShift action_37
action_25 (9) = happyGoto action_41
action_25 (10) = happyGoto action_17
action_25 _ = happyFail

action_26 _ = happyReduce_26

action_27 _ = happyReduce_27

action_28 _ = happyReduce_28

action_29 _ = happyReduce_29

action_30 _ = happyReduce_30

action_31 _ = happyReduce_31

action_32 _ = happyReduce_32

action_33 _ = happyReduce_33

action_34 _ = happyReduce_39

action_35 _ = happyReduce_40

action_36 _ = happyReduce_41

action_37 _ = happyReduce_21

action_38 (69) = happyAccept
action_38 _ = happyFail

action_39 (67) = happyShift action_40
action_39 _ = happyFail

action_40 (67) = happyShift action_86
action_40 (7) = happyGoto action_88
action_40 _ = happyReduce_7

action_41 (14) = happyShift action_18
action_41 (16) = happyShift action_19
action_41 (18) = happyShift action_20
action_41 (25) = happyShift action_21
action_41 (27) = happyShift action_51
action_41 (28) = happyShift action_52
action_41 (29) = happyShift action_53
action_41 (30) = happyShift action_54
action_41 (31) = happyShift action_55
action_41 (32) = happyShift action_56
action_41 (33) = happyShift action_57
action_41 (34) = happyShift action_58
action_41 (35) = happyShift action_59
action_41 (36) = happyShift action_60
action_41 (37) = happyShift action_61
action_41 (38) = happyShift action_62
action_41 (39) = happyShift action_63
action_41 (40) = happyShift action_22
action_41 (41) = happyShift action_64
action_41 (42) = happyShift action_23
action_41 (43) = happyShift action_24
action_41 (45) = happyShift action_25
action_41 (46) = happyShift action_87
action_41 (48) = happyShift action_26
action_41 (49) = happyShift action_27
action_41 (50) = happyShift action_28
action_41 (51) = happyShift action_29
action_41 (52) = happyShift action_30
action_41 (53) = happyShift action_31
action_41 (54) = happyShift action_32
action_41 (55) = happyShift action_33
action_41 (56) = happyShift action_65
action_41 (64) = happyShift action_34
action_41 (65) = happyShift action_35
action_41 (66) = happyShift action_36
action_41 (67) = happyShift action_37
action_41 (9) = happyGoto action_49
action_41 (10) = happyGoto action_17
action_41 (12) = happyGoto action_50
action_41 _ = happyFail

action_42 (67) = happyShift action_86
action_42 (7) = happyGoto action_85
action_42 _ = happyReduce_7

action_43 (26) = happyShift action_84
action_43 _ = happyFail

action_44 (14) = happyShift action_18
action_44 (16) = happyShift action_19
action_44 (18) = happyShift action_20
action_44 (21) = happyShift action_83
action_44 (25) = happyShift action_21
action_44 (27) = happyShift action_51
action_44 (28) = happyShift action_52
action_44 (29) = happyShift action_53
action_44 (30) = happyShift action_54
action_44 (31) = happyShift action_55
action_44 (32) = happyShift action_56
action_44 (33) = happyShift action_57
action_44 (34) = happyShift action_58
action_44 (35) = happyShift action_59
action_44 (36) = happyShift action_60
action_44 (37) = happyShift action_61
action_44 (38) = happyShift action_62
action_44 (39) = happyShift action_63
action_44 (40) = happyShift action_22
action_44 (41) = happyShift action_64
action_44 (42) = happyShift action_23
action_44 (43) = happyShift action_24
action_44 (45) = happyShift action_25
action_44 (48) = happyShift action_26
action_44 (49) = happyShift action_27
action_44 (50) = happyShift action_28
action_44 (51) = happyShift action_29
action_44 (52) = happyShift action_30
action_44 (53) = happyShift action_31
action_44 (54) = happyShift action_32
action_44 (55) = happyShift action_33
action_44 (56) = happyShift action_65
action_44 (64) = happyShift action_34
action_44 (65) = happyShift action_35
action_44 (66) = happyShift action_36
action_44 (67) = happyShift action_37
action_44 (9) = happyGoto action_49
action_44 (10) = happyGoto action_17
action_44 (12) = happyGoto action_50
action_44 _ = happyFail

action_45 (21) = happyShift action_82
action_45 _ = happyFail

action_46 (14) = happyShift action_18
action_46 (16) = happyShift action_19
action_46 (17) = happyShift action_80
action_46 (18) = happyShift action_20
action_46 (21) = happyShift action_81
action_46 (25) = happyShift action_21
action_46 (27) = happyShift action_51
action_46 (28) = happyShift action_52
action_46 (29) = happyShift action_53
action_46 (30) = happyShift action_54
action_46 (31) = happyShift action_55
action_46 (32) = happyShift action_56
action_46 (33) = happyShift action_57
action_46 (34) = happyShift action_58
action_46 (35) = happyShift action_59
action_46 (36) = happyShift action_60
action_46 (37) = happyShift action_61
action_46 (38) = happyShift action_62
action_46 (39) = happyShift action_63
action_46 (40) = happyShift action_22
action_46 (41) = happyShift action_64
action_46 (42) = happyShift action_23
action_46 (43) = happyShift action_24
action_46 (45) = happyShift action_25
action_46 (48) = happyShift action_26
action_46 (49) = happyShift action_27
action_46 (50) = happyShift action_28
action_46 (51) = happyShift action_29
action_46 (52) = happyShift action_30
action_46 (53) = happyShift action_31
action_46 (54) = happyShift action_32
action_46 (55) = happyShift action_33
action_46 (56) = happyShift action_65
action_46 (64) = happyShift action_34
action_46 (65) = happyShift action_35
action_46 (66) = happyShift action_36
action_46 (67) = happyShift action_37
action_46 (9) = happyGoto action_49
action_46 (10) = happyGoto action_17
action_46 (11) = happyGoto action_79
action_46 (12) = happyGoto action_50
action_46 _ = happyFail

action_47 _ = happyReduce_42

action_48 (14) = happyShift action_18
action_48 (15) = happyShift action_78
action_48 (16) = happyShift action_19
action_48 (18) = happyShift action_20
action_48 (25) = happyShift action_21
action_48 (27) = happyShift action_51
action_48 (28) = happyShift action_52
action_48 (29) = happyShift action_53
action_48 (30) = happyShift action_54
action_48 (31) = happyShift action_55
action_48 (32) = happyShift action_56
action_48 (33) = happyShift action_57
action_48 (34) = happyShift action_58
action_48 (35) = happyShift action_59
action_48 (36) = happyShift action_60
action_48 (37) = happyShift action_61
action_48 (38) = happyShift action_62
action_48 (39) = happyShift action_63
action_48 (40) = happyShift action_22
action_48 (41) = happyShift action_64
action_48 (42) = happyShift action_23
action_48 (43) = happyShift action_24
action_48 (45) = happyShift action_25
action_48 (48) = happyShift action_26
action_48 (49) = happyShift action_27
action_48 (50) = happyShift action_28
action_48 (51) = happyShift action_29
action_48 (52) = happyShift action_30
action_48 (53) = happyShift action_31
action_48 (54) = happyShift action_32
action_48 (55) = happyShift action_33
action_48 (56) = happyShift action_65
action_48 (64) = happyShift action_34
action_48 (65) = happyShift action_35
action_48 (66) = happyShift action_36
action_48 (67) = happyShift action_37
action_48 (9) = happyGoto action_49
action_48 (10) = happyGoto action_17
action_48 (12) = happyGoto action_50
action_48 _ = happyFail

action_49 (25) = happyShift action_21
action_49 (50) = happyShift action_28
action_49 (9) = happyGoto action_49
action_49 (10) = happyGoto action_17
action_49 (12) = happyGoto action_50
action_49 _ = happyReduce_20

action_50 (14) = happyShift action_18
action_50 (16) = happyShift action_19
action_50 (18) = happyShift action_20
action_50 (25) = happyShift action_21
action_50 (40) = happyShift action_22
action_50 (42) = happyShift action_23
action_50 (43) = happyShift action_24
action_50 (45) = happyShift action_25
action_50 (48) = happyShift action_26
action_50 (49) = happyShift action_27
action_50 (50) = happyShift action_28
action_50 (51) = happyShift action_29
action_50 (52) = happyShift action_30
action_50 (53) = happyShift action_31
action_50 (54) = happyShift action_32
action_50 (55) = happyShift action_33
action_50 (64) = happyShift action_34
action_50 (65) = happyShift action_35
action_50 (66) = happyShift action_36
action_50 (67) = happyShift action_37
action_50 (9) = happyGoto action_77
action_50 (10) = happyGoto action_17
action_50 _ = happyFail

action_51 _ = happyReduce_46

action_52 _ = happyReduce_47

action_53 _ = happyReduce_48

action_54 _ = happyReduce_49

action_55 _ = happyReduce_50

action_56 _ = happyReduce_56

action_57 _ = happyReduce_55

action_58 _ = happyReduce_54

action_59 _ = happyReduce_59

action_60 _ = happyReduce_58

action_61 _ = happyReduce_57

action_62 _ = happyReduce_51

action_63 _ = happyReduce_52

action_64 _ = happyReduce_53

action_65 _ = happyReduce_60

action_66 (61) = happyShift action_75
action_66 (62) = happyShift action_76
action_66 (63) = happyShift action_69
action_66 _ = happyFail

action_67 (17) = happyShift action_74
action_67 (63) = happyShift action_69
action_67 _ = happyFail

action_68 (15) = happyShift action_73
action_68 (63) = happyShift action_69
action_68 _ = happyFail

action_69 (14) = happyShift action_7
action_69 (16) = happyShift action_8
action_69 (18) = happyShift action_9
action_69 (57) = happyShift action_10
action_69 (58) = happyShift action_11
action_69 (59) = happyShift action_12
action_69 (60) = happyShift action_13
action_69 (67) = happyShift action_14
action_69 (68) = happyShift action_15
action_69 (8) = happyGoto action_72
action_69 (13) = happyGoto action_6
action_69 _ = happyFail

action_70 (20) = happyShift action_71
action_70 _ = happyFail

action_71 (14) = happyShift action_7
action_71 (16) = happyShift action_8
action_71 (18) = happyShift action_9
action_71 (57) = happyShift action_10
action_71 (58) = happyShift action_11
action_71 (59) = happyShift action_12
action_71 (60) = happyShift action_13
action_71 (67) = happyShift action_14
action_71 (68) = happyShift action_15
action_71 (8) = happyGoto action_100
action_71 (13) = happyGoto action_6
action_71 _ = happyFail

action_72 (63) = happyShift action_69
action_72 _ = happyReduce_15

action_73 _ = happyReduce_16

action_74 _ = happyReduce_12

action_75 (14) = happyShift action_7
action_75 (16) = happyShift action_8
action_75 (18) = happyShift action_9
action_75 (57) = happyShift action_10
action_75 (58) = happyShift action_11
action_75 (59) = happyShift action_12
action_75 (60) = happyShift action_13
action_75 (67) = happyShift action_14
action_75 (68) = happyShift action_15
action_75 (8) = happyGoto action_99
action_75 (13) = happyGoto action_6
action_75 _ = happyFail

action_76 (14) = happyShift action_7
action_76 (16) = happyShift action_8
action_76 (18) = happyShift action_9
action_76 (57) = happyShift action_10
action_76 (58) = happyShift action_11
action_76 (59) = happyShift action_12
action_76 (60) = happyShift action_13
action_76 (67) = happyShift action_14
action_76 (68) = happyShift action_15
action_76 (8) = happyGoto action_98
action_76 (13) = happyGoto action_6
action_76 _ = happyFail

action_77 (14) = happyShift action_18
action_77 (16) = happyShift action_19
action_77 (18) = happyShift action_20
action_77 (25) = happyShift action_21
action_77 (27) = happyShift action_51
action_77 (28) = happyShift action_52
action_77 (29) = happyShift action_53
action_77 (30) = happyShift action_54
action_77 (31) = happyShift action_55
action_77 (32) = happyShift action_56
action_77 (33) = happyShift action_57
action_77 (34) = happyShift action_58
action_77 (35) = happyShift action_59
action_77 (36) = happyShift action_60
action_77 (37) = happyShift action_61
action_77 (38) = happyShift action_62
action_77 (39) = happyShift action_63
action_77 (40) = happyShift action_22
action_77 (41) = happyShift action_64
action_77 (42) = happyShift action_23
action_77 (43) = happyShift action_24
action_77 (45) = happyShift action_25
action_77 (48) = happyShift action_26
action_77 (49) = happyShift action_27
action_77 (50) = happyShift action_28
action_77 (51) = happyShift action_29
action_77 (52) = happyShift action_30
action_77 (53) = happyShift action_31
action_77 (54) = happyShift action_32
action_77 (55) = happyShift action_33
action_77 (56) = happyShift action_65
action_77 (64) = happyShift action_34
action_77 (65) = happyShift action_35
action_77 (66) = happyShift action_36
action_77 (67) = happyShift action_37
action_77 (9) = happyGoto action_49
action_77 (10) = happyGoto action_17
action_77 (12) = happyGoto action_50
action_77 _ = happyReduce_23

action_78 _ = happyReduce_35

action_79 _ = happyReduce_43

action_80 _ = happyReduce_45

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
action_81 (55) = happyShift action_33
action_81 (64) = happyShift action_34
action_81 (65) = happyShift action_35
action_81 (66) = happyShift action_36
action_81 (67) = happyShift action_37
action_81 (9) = happyGoto action_97
action_81 (10) = happyGoto action_17
action_81 _ = happyFail

action_82 (14) = happyShift action_18
action_82 (16) = happyShift action_19
action_82 (18) = happyShift action_20
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
action_82 (55) = happyShift action_33
action_82 (64) = happyShift action_34
action_82 (65) = happyShift action_35
action_82 (66) = happyShift action_36
action_82 (67) = happyShift action_37
action_82 (9) = happyGoto action_96
action_82 (10) = happyGoto action_17
action_82 _ = happyFail

action_83 (14) = happyShift action_18
action_83 (16) = happyShift action_19
action_83 (18) = happyShift action_20
action_83 (22) = happyShift action_95
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
action_83 (55) = happyShift action_33
action_83 (64) = happyShift action_34
action_83 (65) = happyShift action_35
action_83 (66) = happyShift action_36
action_83 (67) = happyShift action_37
action_83 (9) = happyGoto action_94
action_83 (10) = happyGoto action_17
action_83 _ = happyFail

action_84 (14) = happyShift action_18
action_84 (16) = happyShift action_19
action_84 (18) = happyShift action_20
action_84 (25) = happyShift action_21
action_84 (40) = happyShift action_22
action_84 (42) = happyShift action_23
action_84 (43) = happyShift action_24
action_84 (45) = happyShift action_25
action_84 (48) = happyShift action_26
action_84 (49) = happyShift action_27
action_84 (50) = happyShift action_28
action_84 (51) = happyShift action_29
action_84 (52) = happyShift action_30
action_84 (53) = happyShift action_31
action_84 (54) = happyShift action_32
action_84 (55) = happyShift action_33
action_84 (64) = happyShift action_34
action_84 (65) = happyShift action_35
action_84 (66) = happyShift action_36
action_84 (67) = happyShift action_37
action_84 (9) = happyGoto action_93
action_84 (10) = happyGoto action_17
action_84 _ = happyFail

action_85 (20) = happyShift action_92
action_85 _ = happyFail

action_86 (67) = happyShift action_86
action_86 (7) = happyGoto action_91
action_86 _ = happyReduce_7

action_87 (14) = happyShift action_18
action_87 (16) = happyShift action_19
action_87 (18) = happyShift action_20
action_87 (25) = happyShift action_21
action_87 (40) = happyShift action_22
action_87 (42) = happyShift action_23
action_87 (43) = happyShift action_24
action_87 (45) = happyShift action_25
action_87 (48) = happyShift action_26
action_87 (49) = happyShift action_27
action_87 (50) = happyShift action_28
action_87 (51) = happyShift action_29
action_87 (52) = happyShift action_30
action_87 (53) = happyShift action_31
action_87 (54) = happyShift action_32
action_87 (55) = happyShift action_33
action_87 (64) = happyShift action_34
action_87 (65) = happyShift action_35
action_87 (66) = happyShift action_36
action_87 (67) = happyShift action_37
action_87 (9) = happyGoto action_90
action_87 (10) = happyGoto action_17
action_87 _ = happyFail

action_88 (20) = happyShift action_89
action_88 _ = happyFail

action_89 (14) = happyShift action_18
action_89 (16) = happyShift action_19
action_89 (18) = happyShift action_20
action_89 (25) = happyShift action_21
action_89 (40) = happyShift action_22
action_89 (42) = happyShift action_23
action_89 (43) = happyShift action_24
action_89 (45) = happyShift action_25
action_89 (48) = happyShift action_26
action_89 (49) = happyShift action_27
action_89 (50) = happyShift action_28
action_89 (51) = happyShift action_29
action_89 (52) = happyShift action_30
action_89 (53) = happyShift action_31
action_89 (54) = happyShift action_32
action_89 (55) = happyShift action_33
action_89 (64) = happyShift action_34
action_89 (65) = happyShift action_35
action_89 (66) = happyShift action_36
action_89 (67) = happyShift action_37
action_89 (9) = happyGoto action_110
action_89 (10) = happyGoto action_17
action_89 _ = happyFail

action_90 (14) = happyShift action_18
action_90 (16) = happyShift action_19
action_90 (18) = happyShift action_20
action_90 (25) = happyShift action_21
action_90 (27) = happyShift action_51
action_90 (28) = happyShift action_52
action_90 (29) = happyShift action_53
action_90 (30) = happyShift action_54
action_90 (31) = happyShift action_55
action_90 (32) = happyShift action_56
action_90 (33) = happyShift action_57
action_90 (34) = happyShift action_58
action_90 (35) = happyShift action_59
action_90 (36) = happyShift action_60
action_90 (37) = happyShift action_61
action_90 (38) = happyShift action_62
action_90 (39) = happyShift action_63
action_90 (40) = happyShift action_22
action_90 (41) = happyShift action_64
action_90 (42) = happyShift action_23
action_90 (43) = happyShift action_24
action_90 (45) = happyShift action_25
action_90 (47) = happyShift action_109
action_90 (48) = happyShift action_26
action_90 (49) = happyShift action_27
action_90 (50) = happyShift action_28
action_90 (51) = happyShift action_29
action_90 (52) = happyShift action_30
action_90 (53) = happyShift action_31
action_90 (54) = happyShift action_32
action_90 (55) = happyShift action_33
action_90 (56) = happyShift action_65
action_90 (64) = happyShift action_34
action_90 (65) = happyShift action_35
action_90 (66) = happyShift action_36
action_90 (67) = happyShift action_37
action_90 (9) = happyGoto action_49
action_90 (10) = happyGoto action_17
action_90 (12) = happyGoto action_50
action_90 _ = happyFail

action_91 _ = happyReduce_6

action_92 (14) = happyShift action_18
action_92 (16) = happyShift action_19
action_92 (18) = happyShift action_20
action_92 (25) = happyShift action_21
action_92 (40) = happyShift action_22
action_92 (42) = happyShift action_23
action_92 (43) = happyShift action_24
action_92 (45) = happyShift action_25
action_92 (48) = happyShift action_26
action_92 (49) = happyShift action_27
action_92 (50) = happyShift action_28
action_92 (51) = happyShift action_29
action_92 (52) = happyShift action_30
action_92 (53) = happyShift action_31
action_92 (54) = happyShift action_32
action_92 (55) = happyShift action_33
action_92 (64) = happyShift action_34
action_92 (65) = happyShift action_35
action_92 (66) = happyShift action_36
action_92 (67) = happyShift action_37
action_92 (9) = happyGoto action_108
action_92 (10) = happyGoto action_17
action_92 _ = happyFail

action_93 (25) = happyShift action_21
action_93 (27) = happyShift action_51
action_93 (28) = happyShift action_52
action_93 (29) = happyShift action_53
action_93 (30) = happyShift action_54
action_93 (31) = happyShift action_55
action_93 (32) = happyShift action_56
action_93 (33) = happyShift action_57
action_93 (34) = happyShift action_58
action_93 (35) = happyShift action_59
action_93 (36) = happyShift action_60
action_93 (37) = happyShift action_61
action_93 (38) = happyShift action_62
action_93 (39) = happyShift action_63
action_93 (41) = happyShift action_64
action_93 (43) = happyFail
action_93 (45) = happyFail
action_93 (50) = happyShift action_28
action_93 (56) = happyShift action_65
action_93 (64) = happyFail
action_93 (65) = happyFail
action_93 (66) = happyFail
action_93 (67) = happyShift action_37
action_93 (9) = happyGoto action_49
action_93 (10) = happyGoto action_17
action_93 (12) = happyGoto action_50
action_93 _ = happyReduce_19

action_94 (14) = happyShift action_18
action_94 (16) = happyShift action_19
action_94 (18) = happyShift action_20
action_94 (19) = happyShift action_107
action_94 (25) = happyShift action_21
action_94 (27) = happyShift action_51
action_94 (28) = happyShift action_52
action_94 (29) = happyShift action_53
action_94 (30) = happyShift action_54
action_94 (31) = happyShift action_55
action_94 (32) = happyShift action_56
action_94 (33) = happyShift action_57
action_94 (34) = happyShift action_58
action_94 (35) = happyShift action_59
action_94 (36) = happyShift action_60
action_94 (37) = happyShift action_61
action_94 (38) = happyShift action_62
action_94 (39) = happyShift action_63
action_94 (40) = happyShift action_22
action_94 (41) = happyShift action_64
action_94 (42) = happyShift action_23
action_94 (43) = happyShift action_24
action_94 (45) = happyShift action_25
action_94 (48) = happyShift action_26
action_94 (49) = happyShift action_27
action_94 (50) = happyShift action_28
action_94 (51) = happyShift action_29
action_94 (52) = happyShift action_30
action_94 (53) = happyShift action_31
action_94 (54) = happyShift action_32
action_94 (55) = happyShift action_33
action_94 (56) = happyShift action_65
action_94 (64) = happyShift action_34
action_94 (65) = happyShift action_35
action_94 (66) = happyShift action_36
action_94 (67) = happyShift action_37
action_94 (9) = happyGoto action_49
action_94 (10) = happyGoto action_17
action_94 (12) = happyGoto action_50
action_94 _ = happyFail

action_95 (19) = happyShift action_106
action_95 _ = happyFail

action_96 (14) = happyShift action_18
action_96 (16) = happyShift action_19
action_96 (18) = happyShift action_20
action_96 (19) = happyShift action_105
action_96 (25) = happyShift action_21
action_96 (27) = happyShift action_51
action_96 (28) = happyShift action_52
action_96 (29) = happyShift action_53
action_96 (30) = happyShift action_54
action_96 (31) = happyShift action_55
action_96 (32) = happyShift action_56
action_96 (33) = happyShift action_57
action_96 (34) = happyShift action_58
action_96 (35) = happyShift action_59
action_96 (36) = happyShift action_60
action_96 (37) = happyShift action_61
action_96 (38) = happyShift action_62
action_96 (39) = happyShift action_63
action_96 (40) = happyShift action_22
action_96 (41) = happyShift action_64
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
action_96 (55) = happyShift action_33
action_96 (56) = happyShift action_65
action_96 (64) = happyShift action_34
action_96 (65) = happyShift action_35
action_96 (66) = happyShift action_36
action_96 (67) = happyShift action_37
action_96 (9) = happyGoto action_49
action_96 (10) = happyGoto action_17
action_96 (12) = happyGoto action_50
action_96 _ = happyFail

action_97 (14) = happyShift action_18
action_97 (16) = happyShift action_19
action_97 (17) = happyShift action_80
action_97 (18) = happyShift action_20
action_97 (21) = happyShift action_81
action_97 (25) = happyShift action_21
action_97 (27) = happyShift action_51
action_97 (28) = happyShift action_52
action_97 (29) = happyShift action_53
action_97 (30) = happyShift action_54
action_97 (31) = happyShift action_55
action_97 (32) = happyShift action_56
action_97 (33) = happyShift action_57
action_97 (34) = happyShift action_58
action_97 (35) = happyShift action_59
action_97 (36) = happyShift action_60
action_97 (37) = happyShift action_61
action_97 (38) = happyShift action_62
action_97 (39) = happyShift action_63
action_97 (40) = happyShift action_22
action_97 (41) = happyShift action_64
action_97 (42) = happyShift action_23
action_97 (43) = happyShift action_24
action_97 (45) = happyShift action_25
action_97 (48) = happyShift action_26
action_97 (49) = happyShift action_27
action_97 (50) = happyShift action_28
action_97 (51) = happyShift action_29
action_97 (52) = happyShift action_30
action_97 (53) = happyShift action_31
action_97 (54) = happyShift action_32
action_97 (55) = happyShift action_33
action_97 (56) = happyShift action_65
action_97 (64) = happyShift action_34
action_97 (65) = happyShift action_35
action_97 (66) = happyShift action_36
action_97 (67) = happyShift action_37
action_97 (9) = happyGoto action_49
action_97 (10) = happyGoto action_17
action_97 (11) = happyGoto action_104
action_97 (12) = happyGoto action_50
action_97 _ = happyFail

action_98 (19) = happyShift action_103
action_98 (63) = happyShift action_69
action_98 _ = happyFail

action_99 (19) = happyShift action_102
action_99 (63) = happyShift action_69
action_99 _ = happyFail

action_100 (23) = happyShift action_39
action_100 (24) = happyShift action_4
action_100 (63) = happyShift action_69
action_100 (6) = happyGoto action_101
action_100 _ = happyReduce_5

action_101 _ = happyReduce_3

action_102 _ = happyReduce_13

action_103 _ = happyReduce_14

action_104 _ = happyReduce_44

action_105 _ = happyReduce_37

action_106 _ = happyReduce_36

action_107 _ = happyReduce_38

action_108 (14) = happyShift action_18
action_108 (16) = happyShift action_19
action_108 (18) = happyShift action_20
action_108 (25) = happyShift action_21
action_108 (27) = happyShift action_51
action_108 (28) = happyShift action_52
action_108 (29) = happyShift action_53
action_108 (30) = happyShift action_54
action_108 (31) = happyShift action_55
action_108 (32) = happyShift action_56
action_108 (33) = happyShift action_57
action_108 (34) = happyShift action_58
action_108 (35) = happyShift action_59
action_108 (36) = happyShift action_60
action_108 (37) = happyShift action_61
action_108 (38) = happyShift action_62
action_108 (39) = happyShift action_63
action_108 (40) = happyShift action_22
action_108 (41) = happyShift action_64
action_108 (42) = happyShift action_23
action_108 (43) = happyShift action_24
action_108 (44) = happyShift action_113
action_108 (45) = happyShift action_25
action_108 (48) = happyShift action_26
action_108 (49) = happyShift action_27
action_108 (50) = happyShift action_28
action_108 (51) = happyShift action_29
action_108 (52) = happyShift action_30
action_108 (53) = happyShift action_31
action_108 (54) = happyShift action_32
action_108 (55) = happyShift action_33
action_108 (56) = happyShift action_65
action_108 (64) = happyShift action_34
action_108 (65) = happyShift action_35
action_108 (66) = happyShift action_36
action_108 (67) = happyShift action_37
action_108 (9) = happyGoto action_49
action_108 (10) = happyGoto action_17
action_108 (12) = happyGoto action_50
action_108 _ = happyFail

action_109 (14) = happyShift action_18
action_109 (16) = happyShift action_19
action_109 (18) = happyShift action_20
action_109 (25) = happyShift action_21
action_109 (40) = happyShift action_22
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
action_109 (55) = happyShift action_33
action_109 (64) = happyShift action_34
action_109 (65) = happyShift action_35
action_109 (66) = happyShift action_36
action_109 (67) = happyShift action_37
action_109 (9) = happyGoto action_112
action_109 (10) = happyGoto action_17
action_109 _ = happyFail

action_110 (14) = happyShift action_18
action_110 (16) = happyShift action_19
action_110 (18) = happyShift action_20
action_110 (23) = happyShift action_39
action_110 (24) = happyShift action_4
action_110 (25) = happyShift action_21
action_110 (27) = happyShift action_51
action_110 (28) = happyShift action_52
action_110 (29) = happyShift action_53
action_110 (30) = happyShift action_54
action_110 (31) = happyShift action_55
action_110 (32) = happyShift action_56
action_110 (33) = happyShift action_57
action_110 (34) = happyShift action_58
action_110 (35) = happyShift action_59
action_110 (36) = happyShift action_60
action_110 (37) = happyShift action_61
action_110 (38) = happyShift action_62
action_110 (39) = happyShift action_63
action_110 (40) = happyShift action_22
action_110 (41) = happyShift action_64
action_110 (42) = happyShift action_23
action_110 (43) = happyShift action_24
action_110 (45) = happyShift action_25
action_110 (48) = happyShift action_26
action_110 (49) = happyShift action_27
action_110 (50) = happyShift action_28
action_110 (51) = happyShift action_29
action_110 (52) = happyShift action_30
action_110 (53) = happyShift action_31
action_110 (54) = happyShift action_32
action_110 (55) = happyShift action_33
action_110 (56) = happyShift action_65
action_110 (64) = happyShift action_34
action_110 (65) = happyShift action_35
action_110 (66) = happyShift action_36
action_110 (67) = happyShift action_37
action_110 (6) = happyGoto action_111
action_110 (9) = happyGoto action_49
action_110 (10) = happyGoto action_17
action_110 (12) = happyGoto action_50
action_110 _ = happyReduce_5

action_111 _ = happyReduce_4

action_112 (25) = happyShift action_21
action_112 (27) = happyShift action_51
action_112 (28) = happyShift action_52
action_112 (29) = happyShift action_53
action_112 (30) = happyShift action_54
action_112 (31) = happyShift action_55
action_112 (32) = happyShift action_56
action_112 (33) = happyShift action_57
action_112 (34) = happyShift action_58
action_112 (35) = happyShift action_59
action_112 (36) = happyShift action_60
action_112 (37) = happyShift action_61
action_112 (38) = happyShift action_62
action_112 (39) = happyShift action_63
action_112 (41) = happyShift action_64
action_112 (43) = happyFail
action_112 (45) = happyFail
action_112 (50) = happyShift action_28
action_112 (56) = happyShift action_65
action_112 (64) = happyFail
action_112 (65) = happyFail
action_112 (66) = happyFail
action_112 (67) = happyShift action_37
action_112 (9) = happyGoto action_49
action_112 (10) = happyGoto action_17
action_112 (12) = happyGoto action_50
action_112 _ = happyReduce_22

action_113 (14) = happyShift action_18
action_113 (16) = happyShift action_19
action_113 (18) = happyShift action_20
action_113 (25) = happyShift action_21
action_113 (40) = happyShift action_22
action_113 (42) = happyShift action_23
action_113 (43) = happyShift action_24
action_113 (45) = happyShift action_25
action_113 (48) = happyShift action_26
action_113 (49) = happyShift action_27
action_113 (50) = happyShift action_28
action_113 (51) = happyShift action_29
action_113 (52) = happyShift action_30
action_113 (53) = happyShift action_31
action_113 (54) = happyShift action_32
action_113 (55) = happyShift action_33
action_113 (64) = happyShift action_34
action_113 (65) = happyShift action_35
action_113 (66) = happyShift action_36
action_113 (67) = happyShift action_37
action_113 (9) = happyGoto action_114
action_113 (10) = happyGoto action_17
action_113 _ = happyFail

action_114 (25) = happyShift action_21
action_114 (27) = happyShift action_51
action_114 (28) = happyShift action_52
action_114 (29) = happyShift action_53
action_114 (30) = happyShift action_54
action_114 (31) = happyShift action_55
action_114 (32) = happyShift action_56
action_114 (33) = happyShift action_57
action_114 (34) = happyShift action_58
action_114 (35) = happyShift action_59
action_114 (36) = happyShift action_60
action_114 (37) = happyShift action_61
action_114 (38) = happyShift action_62
action_114 (39) = happyShift action_63
action_114 (41) = happyShift action_64
action_114 (43) = happyFail
action_114 (45) = happyFail
action_114 (50) = happyShift action_28
action_114 (56) = happyShift action_65
action_114 (64) = happyFail
action_114 (65) = happyFail
action_114 (66) = happyFail
action_114 (67) = happyShift action_37
action_114 (9) = happyGoto action_49
action_114 (10) = happyGoto action_17
action_114 (12) = happyGoto action_50
action_114 _ = happyReduce_18

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
		 (Operation IsLeft
	)

happyReduce_29 = happySpecReduce_1  9 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn9
		 (Operation Fst
	)

happyReduce_30 = happySpecReduce_1  9 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn9
		 (Operation Snd
	)

happyReduce_31 = happySpecReduce_1  9 happyReduction_31
happyReduction_31 _
	 =  HappyAbsSyn9
		 (Operation Head
	)

happyReduce_32 = happySpecReduce_1  9 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn9
		 (Operation Tail
	)

happyReduce_33 = happySpecReduce_1  9 happyReduction_33
happyReduction_33 _
	 =  HappyAbsSyn9
		 (Operation Null
	)

happyReduce_34 = happySpecReduce_1  9 happyReduction_34
happyReduction_34 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  9 happyReduction_35
happyReduction_35 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happyReduce 5 9 happyReduction_36
happyReduction_36 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (App (Operation InjL) happy_var_2
	) `HappyStk` happyRest

happyReduce_37 = happyReduce 5 9 happyReduction_37
happyReduction_37 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (App (Operation InjR) happy_var_4
	) `HappyStk` happyRest

happyReduce_38 = happyReduce 5 9 happyReduction_38
happyReduction_38 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (App (App (Operation Tuple) happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_39 = happySpecReduce_1  9 happyReduction_39
happyReduction_39 (HappyTerminal (TokenInt    p happy_var_1))
	 =  HappyAbsSyn9
		 (Constant (IntVal happy_var_1)
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  9 happyReduction_40
happyReduction_40 (HappyTerminal (TokenBool   p happy_var_1))
	 =  HappyAbsSyn9
		 (Constant (BoolVal happy_var_1)
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  9 happyReduction_41
happyReduction_41 (HappyTerminal (TokenStr    p happy_var_1))
	 =  HappyAbsSyn9
		 (parseStr happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_2  10 happyReduction_42
happyReduction_42 _
	_
	 =  HappyAbsSyn9
		 (Operation Empty
	)

happyReduce_43 = happySpecReduce_3  10 happyReduction_43
happyReduction_43 (HappyAbsSyn9  happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (App (App (Operation Cons) happy_var_2) happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  11 happyReduction_44
happyReduction_44 (HappyAbsSyn9  happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (App (App (Operation Cons) happy_var_2) happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  11 happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn9
		 (Operation Empty
	)

happyReduce_46 = happySpecReduce_1  12 happyReduction_46
happyReduction_46 _
	 =  HappyAbsSyn9
		 (Operation Add
	)

happyReduce_47 = happySpecReduce_1  12 happyReduction_47
happyReduction_47 _
	 =  HappyAbsSyn9
		 (Operation Sub
	)

happyReduce_48 = happySpecReduce_1  12 happyReduction_48
happyReduction_48 _
	 =  HappyAbsSyn9
		 (Operation Mul
	)

happyReduce_49 = happySpecReduce_1  12 happyReduction_49
happyReduction_49 _
	 =  HappyAbsSyn9
		 (Operation Div
	)

happyReduce_50 = happySpecReduce_1  12 happyReduction_50
happyReduction_50 _
	 =  HappyAbsSyn9
		 (Operation Mod
	)

happyReduce_51 = happySpecReduce_1  12 happyReduction_51
happyReduction_51 _
	 =  HappyAbsSyn9
		 (Operation And
	)

happyReduce_52 = happySpecReduce_1  12 happyReduction_52
happyReduction_52 _
	 =  HappyAbsSyn9
		 (Operation Or
	)

happyReduce_53 = happySpecReduce_1  12 happyReduction_53
happyReduction_53 _
	 =  HappyAbsSyn9
		 (Operation Xor
	)

happyReduce_54 = happySpecReduce_1  12 happyReduction_54
happyReduction_54 _
	 =  HappyAbsSyn9
		 (Operation Lss
	)

happyReduce_55 = happySpecReduce_1  12 happyReduction_55
happyReduction_55 _
	 =  HappyAbsSyn9
		 (Operation LsE
	)

happyReduce_56 = happySpecReduce_1  12 happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn9
		 (Operation Equ
	)

happyReduce_57 = happySpecReduce_1  12 happyReduction_57
happyReduction_57 _
	 =  HappyAbsSyn9
		 (Operation NEq
	)

happyReduce_58 = happySpecReduce_1  12 happyReduction_58
happyReduction_58 _
	 =  HappyAbsSyn9
		 (Operation Gtr
	)

happyReduce_59 = happySpecReduce_1  12 happyReduction_59
happyReduction_59 _
	 =  HappyAbsSyn9
		 (Operation GtE
	)

happyReduce_60 = happySpecReduce_1  12 happyReduction_60
happyReduction_60 _
	 =  HappyAbsSyn9
		 (Operation Cons
	)

happyReduce_61 = happySpecReduce_1  13 happyReduction_61
happyReduction_61 (HappyTerminal (TokenIdLC   p happy_var_1))
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  13 happyReduction_62
happyReduction_62 (HappyTerminal (TokenIdUC   p happy_var_1))
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 69 69 notHappyAtAll (HappyState action) sts stk []

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
	TokenIsLeft p -> cont 50;
	TokenFst    p -> cont 51;
	TokenSnd    p -> cont 52;
	TokenHead   p -> cont 53;
	TokenTail   p -> cont 54;
	TokenNull   p -> cont 55;
	TokenCons   p -> cont 56;
	TokenTyInt  p -> cont 57;
	TokenTyBool p -> cont 58;
	TokenTyChar p -> cont 59;
	TokenTyStr  p -> cont 60;
	TokenTySum  p -> cont 61;
	TokenTyProd p -> cont 62;
	TokenTyArrw p -> cont 63;
	TokenInt    p happy_dollar_dollar -> cont 64;
	TokenBool   p happy_dollar_dollar -> cont 65;
	TokenStr    p happy_dollar_dollar -> cont 66;
	TokenIdLC   p happy_dollar_dollar -> cont 67;
	TokenIdUC   p happy_dollar_dollar -> cont 68;
	_ -> happyError' (tk:tks)
	}

happyError_ 69 tk tks = happyError' tks
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
