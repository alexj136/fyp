{-# OPTIONS_GHC -w #-}
module Parser where

import Lexer
import Syntax
import PostParsing

{-- CONTEXT FREE GRAMMAR

PROG ::= alias ID = TY
       | type ID = TY
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
 action_114,
 action_115,
 action_116,
 action_117,
 action_118 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
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

action_0 (23) = happyShift action_4
action_0 (24) = happyShift action_38
action_0 (25) = happyShift action_39
action_0 (6) = happyGoto action_37
action_0 _ = happyReduce_6

action_1 (14) = happyShift action_18
action_1 (16) = happyShift action_19
action_1 (18) = happyShift action_20
action_1 (26) = happyShift action_21
action_1 (41) = happyShift action_22
action_1 (43) = happyShift action_23
action_1 (44) = happyShift action_24
action_1 (46) = happyShift action_25
action_1 (49) = happyShift action_26
action_1 (50) = happyShift action_27
action_1 (51) = happyShift action_28
action_1 (52) = happyShift action_29
action_1 (53) = happyShift action_30
action_1 (54) = happyShift action_31
action_1 (55) = happyShift action_32
action_1 (64) = happyShift action_33
action_1 (65) = happyShift action_34
action_1 (66) = happyShift action_35
action_1 (67) = happyShift action_36
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

action_3 (23) = happyShift action_4
action_3 _ = happyFail

action_4 (67) = happyShift action_14
action_4 (68) = happyShift action_15
action_4 (13) = happyGoto action_71
action_4 _ = happyFail

action_5 (63) = happyShift action_70
action_5 (69) = happyAccept
action_5 _ = happyFail

action_6 _ = happyReduce_18

action_7 (14) = happyShift action_7
action_7 (16) = happyShift action_8
action_7 (18) = happyShift action_9
action_7 (57) = happyShift action_10
action_7 (58) = happyShift action_11
action_7 (59) = happyShift action_12
action_7 (60) = happyShift action_13
action_7 (67) = happyShift action_14
action_7 (68) = happyShift action_15
action_7 (8) = happyGoto action_69
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
action_8 (8) = happyGoto action_68
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
action_9 (8) = happyGoto action_67
action_9 (13) = happyGoto action_6
action_9 _ = happyFail

action_10 _ = happyReduce_9

action_11 _ = happyReduce_10

action_12 _ = happyReduce_11

action_13 _ = happyReduce_12

action_14 _ = happyReduce_61

action_15 _ = happyReduce_62

action_16 (14) = happyShift action_18
action_16 (16) = happyShift action_19
action_16 (18) = happyShift action_20
action_16 (26) = happyShift action_21
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
action_16 (40) = happyShift action_64
action_16 (41) = happyShift action_22
action_16 (42) = happyShift action_65
action_16 (43) = happyShift action_23
action_16 (44) = happyShift action_24
action_16 (46) = happyShift action_25
action_16 (49) = happyShift action_26
action_16 (50) = happyShift action_27
action_16 (51) = happyShift action_28
action_16 (52) = happyShift action_29
action_16 (53) = happyShift action_30
action_16 (54) = happyShift action_31
action_16 (55) = happyShift action_32
action_16 (56) = happyShift action_66
action_16 (64) = happyShift action_33
action_16 (65) = happyShift action_34
action_16 (66) = happyShift action_35
action_16 (67) = happyShift action_36
action_16 (69) = happyAccept
action_16 (9) = happyGoto action_50
action_16 (10) = happyGoto action_17
action_16 (12) = happyGoto action_51
action_16 _ = happyFail

action_17 _ = happyReduce_34

action_18 (14) = happyShift action_18
action_18 (16) = happyShift action_19
action_18 (18) = happyShift action_20
action_18 (26) = happyShift action_21
action_18 (41) = happyShift action_22
action_18 (43) = happyShift action_23
action_18 (44) = happyShift action_24
action_18 (46) = happyShift action_25
action_18 (49) = happyShift action_26
action_18 (50) = happyShift action_27
action_18 (51) = happyShift action_28
action_18 (52) = happyShift action_29
action_18 (53) = happyShift action_30
action_18 (54) = happyShift action_31
action_18 (55) = happyShift action_32
action_18 (64) = happyShift action_33
action_18 (65) = happyShift action_34
action_18 (66) = happyShift action_35
action_18 (67) = happyShift action_36
action_18 (9) = happyGoto action_49
action_18 (10) = happyGoto action_17
action_18 _ = happyFail

action_19 (14) = happyShift action_18
action_19 (16) = happyShift action_19
action_19 (17) = happyShift action_48
action_19 (18) = happyShift action_20
action_19 (26) = happyShift action_21
action_19 (41) = happyShift action_22
action_19 (43) = happyShift action_23
action_19 (44) = happyShift action_24
action_19 (46) = happyShift action_25
action_19 (49) = happyShift action_26
action_19 (50) = happyShift action_27
action_19 (51) = happyShift action_28
action_19 (52) = happyShift action_29
action_19 (53) = happyShift action_30
action_19 (54) = happyShift action_31
action_19 (55) = happyShift action_32
action_19 (64) = happyShift action_33
action_19 (65) = happyShift action_34
action_19 (66) = happyShift action_35
action_19 (67) = happyShift action_36
action_19 (9) = happyGoto action_47
action_19 (10) = happyGoto action_17
action_19 _ = happyFail

action_20 (14) = happyShift action_18
action_20 (16) = happyShift action_19
action_20 (18) = happyShift action_20
action_20 (22) = happyShift action_46
action_20 (26) = happyShift action_21
action_20 (41) = happyShift action_22
action_20 (43) = happyShift action_23
action_20 (44) = happyShift action_24
action_20 (46) = happyShift action_25
action_20 (49) = happyShift action_26
action_20 (50) = happyShift action_27
action_20 (51) = happyShift action_28
action_20 (52) = happyShift action_29
action_20 (53) = happyShift action_30
action_20 (54) = happyShift action_31
action_20 (55) = happyShift action_32
action_20 (64) = happyShift action_33
action_20 (65) = happyShift action_34
action_20 (66) = happyShift action_35
action_20 (67) = happyShift action_36
action_20 (9) = happyGoto action_45
action_20 (10) = happyGoto action_17
action_20 _ = happyFail

action_21 (67) = happyShift action_44
action_21 _ = happyFail

action_22 _ = happyReduce_26

action_23 _ = happyReduce_25

action_24 (67) = happyShift action_43
action_24 _ = happyFail

action_25 (14) = happyShift action_18
action_25 (16) = happyShift action_19
action_25 (18) = happyShift action_20
action_25 (26) = happyShift action_21
action_25 (41) = happyShift action_22
action_25 (43) = happyShift action_23
action_25 (44) = happyShift action_24
action_25 (46) = happyShift action_25
action_25 (49) = happyShift action_26
action_25 (50) = happyShift action_27
action_25 (51) = happyShift action_28
action_25 (52) = happyShift action_29
action_25 (53) = happyShift action_30
action_25 (54) = happyShift action_31
action_25 (55) = happyShift action_32
action_25 (64) = happyShift action_33
action_25 (65) = happyShift action_34
action_25 (66) = happyShift action_35
action_25 (67) = happyShift action_36
action_25 (9) = happyGoto action_42
action_25 (10) = happyGoto action_17
action_25 _ = happyFail

action_26 _ = happyReduce_27

action_27 _ = happyReduce_28

action_28 _ = happyReduce_29

action_29 _ = happyReduce_30

action_30 _ = happyReduce_31

action_31 _ = happyReduce_32

action_32 _ = happyReduce_33

action_33 _ = happyReduce_39

action_34 _ = happyReduce_40

action_35 _ = happyReduce_41

action_36 _ = happyReduce_22

action_37 (69) = happyAccept
action_37 _ = happyFail

action_38 (67) = happyShift action_41
action_38 _ = happyFail

action_39 (67) = happyShift action_40
action_39 _ = happyFail

action_40 (20) = happyShift action_90
action_40 _ = happyFail

action_41 (67) = happyShift action_87
action_41 (7) = happyGoto action_89
action_41 _ = happyReduce_8

action_42 (14) = happyShift action_18
action_42 (16) = happyShift action_19
action_42 (18) = happyShift action_20
action_42 (26) = happyShift action_21
action_42 (28) = happyShift action_52
action_42 (29) = happyShift action_53
action_42 (30) = happyShift action_54
action_42 (31) = happyShift action_55
action_42 (32) = happyShift action_56
action_42 (33) = happyShift action_57
action_42 (34) = happyShift action_58
action_42 (35) = happyShift action_59
action_42 (36) = happyShift action_60
action_42 (37) = happyShift action_61
action_42 (38) = happyShift action_62
action_42 (39) = happyShift action_63
action_42 (40) = happyShift action_64
action_42 (41) = happyShift action_22
action_42 (42) = happyShift action_65
action_42 (43) = happyShift action_23
action_42 (44) = happyShift action_24
action_42 (46) = happyShift action_25
action_42 (47) = happyShift action_88
action_42 (49) = happyShift action_26
action_42 (50) = happyShift action_27
action_42 (51) = happyShift action_28
action_42 (52) = happyShift action_29
action_42 (53) = happyShift action_30
action_42 (54) = happyShift action_31
action_42 (55) = happyShift action_32
action_42 (56) = happyShift action_66
action_42 (64) = happyShift action_33
action_42 (65) = happyShift action_34
action_42 (66) = happyShift action_35
action_42 (67) = happyShift action_36
action_42 (9) = happyGoto action_50
action_42 (10) = happyGoto action_17
action_42 (12) = happyGoto action_51
action_42 _ = happyFail

action_43 (67) = happyShift action_87
action_43 (7) = happyGoto action_86
action_43 _ = happyReduce_8

action_44 (27) = happyShift action_85
action_44 _ = happyFail

action_45 (14) = happyShift action_18
action_45 (16) = happyShift action_19
action_45 (18) = happyShift action_20
action_45 (21) = happyShift action_84
action_45 (26) = happyShift action_21
action_45 (28) = happyShift action_52
action_45 (29) = happyShift action_53
action_45 (30) = happyShift action_54
action_45 (31) = happyShift action_55
action_45 (32) = happyShift action_56
action_45 (33) = happyShift action_57
action_45 (34) = happyShift action_58
action_45 (35) = happyShift action_59
action_45 (36) = happyShift action_60
action_45 (37) = happyShift action_61
action_45 (38) = happyShift action_62
action_45 (39) = happyShift action_63
action_45 (40) = happyShift action_64
action_45 (41) = happyShift action_22
action_45 (42) = happyShift action_65
action_45 (43) = happyShift action_23
action_45 (44) = happyShift action_24
action_45 (46) = happyShift action_25
action_45 (49) = happyShift action_26
action_45 (50) = happyShift action_27
action_45 (51) = happyShift action_28
action_45 (52) = happyShift action_29
action_45 (53) = happyShift action_30
action_45 (54) = happyShift action_31
action_45 (55) = happyShift action_32
action_45 (56) = happyShift action_66
action_45 (64) = happyShift action_33
action_45 (65) = happyShift action_34
action_45 (66) = happyShift action_35
action_45 (67) = happyShift action_36
action_45 (9) = happyGoto action_50
action_45 (10) = happyGoto action_17
action_45 (12) = happyGoto action_51
action_45 _ = happyFail

action_46 (21) = happyShift action_83
action_46 _ = happyFail

action_47 (14) = happyShift action_18
action_47 (16) = happyShift action_19
action_47 (17) = happyShift action_81
action_47 (18) = happyShift action_20
action_47 (21) = happyShift action_82
action_47 (26) = happyShift action_21
action_47 (28) = happyShift action_52
action_47 (29) = happyShift action_53
action_47 (30) = happyShift action_54
action_47 (31) = happyShift action_55
action_47 (32) = happyShift action_56
action_47 (33) = happyShift action_57
action_47 (34) = happyShift action_58
action_47 (35) = happyShift action_59
action_47 (36) = happyShift action_60
action_47 (37) = happyShift action_61
action_47 (38) = happyShift action_62
action_47 (39) = happyShift action_63
action_47 (40) = happyShift action_64
action_47 (41) = happyShift action_22
action_47 (42) = happyShift action_65
action_47 (43) = happyShift action_23
action_47 (44) = happyShift action_24
action_47 (46) = happyShift action_25
action_47 (49) = happyShift action_26
action_47 (50) = happyShift action_27
action_47 (51) = happyShift action_28
action_47 (52) = happyShift action_29
action_47 (53) = happyShift action_30
action_47 (54) = happyShift action_31
action_47 (55) = happyShift action_32
action_47 (56) = happyShift action_66
action_47 (64) = happyShift action_33
action_47 (65) = happyShift action_34
action_47 (66) = happyShift action_35
action_47 (67) = happyShift action_36
action_47 (9) = happyGoto action_50
action_47 (10) = happyGoto action_17
action_47 (11) = happyGoto action_80
action_47 (12) = happyGoto action_51
action_47 _ = happyFail

action_48 _ = happyReduce_42

action_49 (14) = happyShift action_18
action_49 (15) = happyShift action_79
action_49 (16) = happyShift action_19
action_49 (18) = happyShift action_20
action_49 (26) = happyShift action_21
action_49 (28) = happyShift action_52
action_49 (29) = happyShift action_53
action_49 (30) = happyShift action_54
action_49 (31) = happyShift action_55
action_49 (32) = happyShift action_56
action_49 (33) = happyShift action_57
action_49 (34) = happyShift action_58
action_49 (35) = happyShift action_59
action_49 (36) = happyShift action_60
action_49 (37) = happyShift action_61
action_49 (38) = happyShift action_62
action_49 (39) = happyShift action_63
action_49 (40) = happyShift action_64
action_49 (41) = happyShift action_22
action_49 (42) = happyShift action_65
action_49 (43) = happyShift action_23
action_49 (44) = happyShift action_24
action_49 (46) = happyShift action_25
action_49 (49) = happyShift action_26
action_49 (50) = happyShift action_27
action_49 (51) = happyShift action_28
action_49 (52) = happyShift action_29
action_49 (53) = happyShift action_30
action_49 (54) = happyShift action_31
action_49 (55) = happyShift action_32
action_49 (56) = happyShift action_66
action_49 (64) = happyShift action_33
action_49 (65) = happyShift action_34
action_49 (66) = happyShift action_35
action_49 (67) = happyShift action_36
action_49 (9) = happyGoto action_50
action_49 (10) = happyGoto action_17
action_49 (12) = happyGoto action_51
action_49 _ = happyFail

action_50 (26) = happyShift action_21
action_50 (9) = happyGoto action_50
action_50 (10) = happyGoto action_17
action_50 (12) = happyGoto action_51
action_50 _ = happyReduce_21

action_51 (14) = happyShift action_18
action_51 (16) = happyShift action_19
action_51 (18) = happyShift action_20
action_51 (26) = happyShift action_21
action_51 (41) = happyShift action_22
action_51 (43) = happyShift action_23
action_51 (44) = happyShift action_24
action_51 (46) = happyShift action_25
action_51 (49) = happyShift action_26
action_51 (50) = happyShift action_27
action_51 (51) = happyShift action_28
action_51 (52) = happyShift action_29
action_51 (53) = happyShift action_30
action_51 (54) = happyShift action_31
action_51 (55) = happyShift action_32
action_51 (64) = happyShift action_33
action_51 (65) = happyShift action_34
action_51 (66) = happyShift action_35
action_51 (67) = happyShift action_36
action_51 (9) = happyGoto action_78
action_51 (10) = happyGoto action_17
action_51 _ = happyFail

action_52 _ = happyReduce_46

action_53 _ = happyReduce_47

action_54 _ = happyReduce_48

action_55 _ = happyReduce_49

action_56 _ = happyReduce_50

action_57 _ = happyReduce_56

action_58 _ = happyReduce_55

action_59 _ = happyReduce_54

action_60 _ = happyReduce_59

action_61 _ = happyReduce_58

action_62 _ = happyReduce_57

action_63 _ = happyReduce_51

action_64 _ = happyReduce_52

action_65 _ = happyReduce_53

action_66 _ = happyReduce_60

action_67 (61) = happyShift action_76
action_67 (62) = happyShift action_77
action_67 (63) = happyShift action_70
action_67 _ = happyFail

action_68 (17) = happyShift action_75
action_68 (63) = happyShift action_70
action_68 _ = happyFail

action_69 (15) = happyShift action_74
action_69 (63) = happyShift action_70
action_69 _ = happyFail

action_70 (14) = happyShift action_7
action_70 (16) = happyShift action_8
action_70 (18) = happyShift action_9
action_70 (57) = happyShift action_10
action_70 (58) = happyShift action_11
action_70 (59) = happyShift action_12
action_70 (60) = happyShift action_13
action_70 (67) = happyShift action_14
action_70 (68) = happyShift action_15
action_70 (8) = happyGoto action_73
action_70 (13) = happyGoto action_6
action_70 _ = happyFail

action_71 (20) = happyShift action_72
action_71 _ = happyFail

action_72 (14) = happyShift action_7
action_72 (16) = happyShift action_8
action_72 (18) = happyShift action_9
action_72 (57) = happyShift action_10
action_72 (58) = happyShift action_11
action_72 (59) = happyShift action_12
action_72 (60) = happyShift action_13
action_72 (67) = happyShift action_14
action_72 (68) = happyShift action_15
action_72 (8) = happyGoto action_103
action_72 (13) = happyGoto action_6
action_72 _ = happyFail

action_73 (63) = happyShift action_70
action_73 _ = happyReduce_16

action_74 _ = happyReduce_17

action_75 _ = happyReduce_13

action_76 (14) = happyShift action_7
action_76 (16) = happyShift action_8
action_76 (18) = happyShift action_9
action_76 (57) = happyShift action_10
action_76 (58) = happyShift action_11
action_76 (59) = happyShift action_12
action_76 (60) = happyShift action_13
action_76 (67) = happyShift action_14
action_76 (68) = happyShift action_15
action_76 (8) = happyGoto action_102
action_76 (13) = happyGoto action_6
action_76 _ = happyFail

action_77 (14) = happyShift action_7
action_77 (16) = happyShift action_8
action_77 (18) = happyShift action_9
action_77 (57) = happyShift action_10
action_77 (58) = happyShift action_11
action_77 (59) = happyShift action_12
action_77 (60) = happyShift action_13
action_77 (67) = happyShift action_14
action_77 (68) = happyShift action_15
action_77 (8) = happyGoto action_101
action_77 (13) = happyGoto action_6
action_77 _ = happyFail

action_78 (14) = happyShift action_18
action_78 (16) = happyShift action_19
action_78 (18) = happyShift action_20
action_78 (26) = happyShift action_21
action_78 (28) = happyShift action_52
action_78 (29) = happyShift action_53
action_78 (30) = happyShift action_54
action_78 (31) = happyShift action_55
action_78 (32) = happyShift action_56
action_78 (33) = happyShift action_57
action_78 (34) = happyShift action_58
action_78 (35) = happyShift action_59
action_78 (36) = happyShift action_60
action_78 (37) = happyShift action_61
action_78 (38) = happyShift action_62
action_78 (39) = happyShift action_63
action_78 (40) = happyShift action_64
action_78 (41) = happyShift action_22
action_78 (42) = happyShift action_65
action_78 (43) = happyShift action_23
action_78 (44) = happyShift action_24
action_78 (46) = happyShift action_25
action_78 (49) = happyShift action_26
action_78 (50) = happyShift action_27
action_78 (51) = happyShift action_28
action_78 (52) = happyShift action_29
action_78 (53) = happyShift action_30
action_78 (54) = happyShift action_31
action_78 (55) = happyShift action_32
action_78 (56) = happyShift action_66
action_78 (64) = happyShift action_33
action_78 (65) = happyShift action_34
action_78 (66) = happyShift action_35
action_78 (67) = happyShift action_36
action_78 (9) = happyGoto action_50
action_78 (10) = happyGoto action_17
action_78 (12) = happyGoto action_51
action_78 _ = happyReduce_24

action_79 _ = happyReduce_35

action_80 _ = happyReduce_43

action_81 _ = happyReduce_45

action_82 (14) = happyShift action_18
action_82 (16) = happyShift action_19
action_82 (18) = happyShift action_20
action_82 (26) = happyShift action_21
action_82 (41) = happyShift action_22
action_82 (43) = happyShift action_23
action_82 (44) = happyShift action_24
action_82 (46) = happyShift action_25
action_82 (49) = happyShift action_26
action_82 (50) = happyShift action_27
action_82 (51) = happyShift action_28
action_82 (52) = happyShift action_29
action_82 (53) = happyShift action_30
action_82 (54) = happyShift action_31
action_82 (55) = happyShift action_32
action_82 (64) = happyShift action_33
action_82 (65) = happyShift action_34
action_82 (66) = happyShift action_35
action_82 (67) = happyShift action_36
action_82 (9) = happyGoto action_100
action_82 (10) = happyGoto action_17
action_82 _ = happyFail

action_83 (14) = happyShift action_18
action_83 (16) = happyShift action_19
action_83 (18) = happyShift action_20
action_83 (26) = happyShift action_21
action_83 (41) = happyShift action_22
action_83 (43) = happyShift action_23
action_83 (44) = happyShift action_24
action_83 (46) = happyShift action_25
action_83 (49) = happyShift action_26
action_83 (50) = happyShift action_27
action_83 (51) = happyShift action_28
action_83 (52) = happyShift action_29
action_83 (53) = happyShift action_30
action_83 (54) = happyShift action_31
action_83 (55) = happyShift action_32
action_83 (64) = happyShift action_33
action_83 (65) = happyShift action_34
action_83 (66) = happyShift action_35
action_83 (67) = happyShift action_36
action_83 (9) = happyGoto action_99
action_83 (10) = happyGoto action_17
action_83 _ = happyFail

action_84 (14) = happyShift action_18
action_84 (16) = happyShift action_19
action_84 (18) = happyShift action_20
action_84 (22) = happyShift action_98
action_84 (26) = happyShift action_21
action_84 (41) = happyShift action_22
action_84 (43) = happyShift action_23
action_84 (44) = happyShift action_24
action_84 (46) = happyShift action_25
action_84 (49) = happyShift action_26
action_84 (50) = happyShift action_27
action_84 (51) = happyShift action_28
action_84 (52) = happyShift action_29
action_84 (53) = happyShift action_30
action_84 (54) = happyShift action_31
action_84 (55) = happyShift action_32
action_84 (64) = happyShift action_33
action_84 (65) = happyShift action_34
action_84 (66) = happyShift action_35
action_84 (67) = happyShift action_36
action_84 (9) = happyGoto action_97
action_84 (10) = happyGoto action_17
action_84 _ = happyFail

action_85 (14) = happyShift action_18
action_85 (16) = happyShift action_19
action_85 (18) = happyShift action_20
action_85 (26) = happyShift action_21
action_85 (41) = happyShift action_22
action_85 (43) = happyShift action_23
action_85 (44) = happyShift action_24
action_85 (46) = happyShift action_25
action_85 (49) = happyShift action_26
action_85 (50) = happyShift action_27
action_85 (51) = happyShift action_28
action_85 (52) = happyShift action_29
action_85 (53) = happyShift action_30
action_85 (54) = happyShift action_31
action_85 (55) = happyShift action_32
action_85 (64) = happyShift action_33
action_85 (65) = happyShift action_34
action_85 (66) = happyShift action_35
action_85 (67) = happyShift action_36
action_85 (9) = happyGoto action_96
action_85 (10) = happyGoto action_17
action_85 _ = happyFail

action_86 (20) = happyShift action_95
action_86 _ = happyFail

action_87 (67) = happyShift action_87
action_87 (7) = happyGoto action_94
action_87 _ = happyReduce_8

action_88 (14) = happyShift action_18
action_88 (16) = happyShift action_19
action_88 (18) = happyShift action_20
action_88 (26) = happyShift action_21
action_88 (41) = happyShift action_22
action_88 (43) = happyShift action_23
action_88 (44) = happyShift action_24
action_88 (46) = happyShift action_25
action_88 (49) = happyShift action_26
action_88 (50) = happyShift action_27
action_88 (51) = happyShift action_28
action_88 (52) = happyShift action_29
action_88 (53) = happyShift action_30
action_88 (54) = happyShift action_31
action_88 (55) = happyShift action_32
action_88 (64) = happyShift action_33
action_88 (65) = happyShift action_34
action_88 (66) = happyShift action_35
action_88 (67) = happyShift action_36
action_88 (9) = happyGoto action_93
action_88 (10) = happyGoto action_17
action_88 _ = happyFail

action_89 (20) = happyShift action_92
action_89 _ = happyFail

action_90 (14) = happyShift action_7
action_90 (16) = happyShift action_8
action_90 (18) = happyShift action_9
action_90 (57) = happyShift action_10
action_90 (58) = happyShift action_11
action_90 (59) = happyShift action_12
action_90 (60) = happyShift action_13
action_90 (67) = happyShift action_14
action_90 (68) = happyShift action_15
action_90 (8) = happyGoto action_91
action_90 (13) = happyGoto action_6
action_90 _ = happyFail

action_91 (23) = happyShift action_4
action_91 (24) = happyShift action_38
action_91 (25) = happyShift action_39
action_91 (63) = happyShift action_70
action_91 (6) = happyGoto action_114
action_91 _ = happyReduce_6

action_92 (14) = happyShift action_18
action_92 (16) = happyShift action_19
action_92 (18) = happyShift action_20
action_92 (26) = happyShift action_21
action_92 (41) = happyShift action_22
action_92 (43) = happyShift action_23
action_92 (44) = happyShift action_24
action_92 (46) = happyShift action_25
action_92 (49) = happyShift action_26
action_92 (50) = happyShift action_27
action_92 (51) = happyShift action_28
action_92 (52) = happyShift action_29
action_92 (53) = happyShift action_30
action_92 (54) = happyShift action_31
action_92 (55) = happyShift action_32
action_92 (64) = happyShift action_33
action_92 (65) = happyShift action_34
action_92 (66) = happyShift action_35
action_92 (67) = happyShift action_36
action_92 (9) = happyGoto action_113
action_92 (10) = happyGoto action_17
action_92 _ = happyFail

action_93 (14) = happyShift action_18
action_93 (16) = happyShift action_19
action_93 (18) = happyShift action_20
action_93 (26) = happyShift action_21
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
action_93 (40) = happyShift action_64
action_93 (41) = happyShift action_22
action_93 (42) = happyShift action_65
action_93 (43) = happyShift action_23
action_93 (44) = happyShift action_24
action_93 (46) = happyShift action_25
action_93 (48) = happyShift action_112
action_93 (49) = happyShift action_26
action_93 (50) = happyShift action_27
action_93 (51) = happyShift action_28
action_93 (52) = happyShift action_29
action_93 (53) = happyShift action_30
action_93 (54) = happyShift action_31
action_93 (55) = happyShift action_32
action_93 (56) = happyShift action_66
action_93 (64) = happyShift action_33
action_93 (65) = happyShift action_34
action_93 (66) = happyShift action_35
action_93 (67) = happyShift action_36
action_93 (9) = happyGoto action_50
action_93 (10) = happyGoto action_17
action_93 (12) = happyGoto action_51
action_93 _ = happyFail

action_94 _ = happyReduce_7

action_95 (14) = happyShift action_18
action_95 (16) = happyShift action_19
action_95 (18) = happyShift action_20
action_95 (26) = happyShift action_21
action_95 (41) = happyShift action_22
action_95 (43) = happyShift action_23
action_95 (44) = happyShift action_24
action_95 (46) = happyShift action_25
action_95 (49) = happyShift action_26
action_95 (50) = happyShift action_27
action_95 (51) = happyShift action_28
action_95 (52) = happyShift action_29
action_95 (53) = happyShift action_30
action_95 (54) = happyShift action_31
action_95 (55) = happyShift action_32
action_95 (64) = happyShift action_33
action_95 (65) = happyShift action_34
action_95 (66) = happyShift action_35
action_95 (67) = happyShift action_36
action_95 (9) = happyGoto action_111
action_95 (10) = happyGoto action_17
action_95 _ = happyFail

action_96 (26) = happyShift action_21
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
action_96 (40) = happyShift action_64
action_96 (42) = happyShift action_65
action_96 (44) = happyFail
action_96 (46) = happyFail
action_96 (56) = happyShift action_66
action_96 (64) = happyFail
action_96 (65) = happyFail
action_96 (66) = happyFail
action_96 (67) = happyShift action_36
action_96 (9) = happyGoto action_50
action_96 (10) = happyGoto action_17
action_96 (12) = happyGoto action_51
action_96 _ = happyReduce_20

action_97 (14) = happyShift action_18
action_97 (16) = happyShift action_19
action_97 (18) = happyShift action_20
action_97 (19) = happyShift action_110
action_97 (26) = happyShift action_21
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
action_97 (40) = happyShift action_64
action_97 (41) = happyShift action_22
action_97 (42) = happyShift action_65
action_97 (43) = happyShift action_23
action_97 (44) = happyShift action_24
action_97 (46) = happyShift action_25
action_97 (49) = happyShift action_26
action_97 (50) = happyShift action_27
action_97 (51) = happyShift action_28
action_97 (52) = happyShift action_29
action_97 (53) = happyShift action_30
action_97 (54) = happyShift action_31
action_97 (55) = happyShift action_32
action_97 (56) = happyShift action_66
action_97 (64) = happyShift action_33
action_97 (65) = happyShift action_34
action_97 (66) = happyShift action_35
action_97 (67) = happyShift action_36
action_97 (9) = happyGoto action_50
action_97 (10) = happyGoto action_17
action_97 (12) = happyGoto action_51
action_97 _ = happyFail

action_98 (19) = happyShift action_109
action_98 _ = happyFail

action_99 (14) = happyShift action_18
action_99 (16) = happyShift action_19
action_99 (18) = happyShift action_20
action_99 (19) = happyShift action_108
action_99 (26) = happyShift action_21
action_99 (28) = happyShift action_52
action_99 (29) = happyShift action_53
action_99 (30) = happyShift action_54
action_99 (31) = happyShift action_55
action_99 (32) = happyShift action_56
action_99 (33) = happyShift action_57
action_99 (34) = happyShift action_58
action_99 (35) = happyShift action_59
action_99 (36) = happyShift action_60
action_99 (37) = happyShift action_61
action_99 (38) = happyShift action_62
action_99 (39) = happyShift action_63
action_99 (40) = happyShift action_64
action_99 (41) = happyShift action_22
action_99 (42) = happyShift action_65
action_99 (43) = happyShift action_23
action_99 (44) = happyShift action_24
action_99 (46) = happyShift action_25
action_99 (49) = happyShift action_26
action_99 (50) = happyShift action_27
action_99 (51) = happyShift action_28
action_99 (52) = happyShift action_29
action_99 (53) = happyShift action_30
action_99 (54) = happyShift action_31
action_99 (55) = happyShift action_32
action_99 (56) = happyShift action_66
action_99 (64) = happyShift action_33
action_99 (65) = happyShift action_34
action_99 (66) = happyShift action_35
action_99 (67) = happyShift action_36
action_99 (9) = happyGoto action_50
action_99 (10) = happyGoto action_17
action_99 (12) = happyGoto action_51
action_99 _ = happyFail

action_100 (14) = happyShift action_18
action_100 (16) = happyShift action_19
action_100 (17) = happyShift action_81
action_100 (18) = happyShift action_20
action_100 (21) = happyShift action_82
action_100 (26) = happyShift action_21
action_100 (28) = happyShift action_52
action_100 (29) = happyShift action_53
action_100 (30) = happyShift action_54
action_100 (31) = happyShift action_55
action_100 (32) = happyShift action_56
action_100 (33) = happyShift action_57
action_100 (34) = happyShift action_58
action_100 (35) = happyShift action_59
action_100 (36) = happyShift action_60
action_100 (37) = happyShift action_61
action_100 (38) = happyShift action_62
action_100 (39) = happyShift action_63
action_100 (40) = happyShift action_64
action_100 (41) = happyShift action_22
action_100 (42) = happyShift action_65
action_100 (43) = happyShift action_23
action_100 (44) = happyShift action_24
action_100 (46) = happyShift action_25
action_100 (49) = happyShift action_26
action_100 (50) = happyShift action_27
action_100 (51) = happyShift action_28
action_100 (52) = happyShift action_29
action_100 (53) = happyShift action_30
action_100 (54) = happyShift action_31
action_100 (55) = happyShift action_32
action_100 (56) = happyShift action_66
action_100 (64) = happyShift action_33
action_100 (65) = happyShift action_34
action_100 (66) = happyShift action_35
action_100 (67) = happyShift action_36
action_100 (9) = happyGoto action_50
action_100 (10) = happyGoto action_17
action_100 (11) = happyGoto action_107
action_100 (12) = happyGoto action_51
action_100 _ = happyFail

action_101 (19) = happyShift action_106
action_101 (63) = happyShift action_70
action_101 _ = happyFail

action_102 (19) = happyShift action_105
action_102 (63) = happyShift action_70
action_102 _ = happyFail

action_103 (23) = happyShift action_4
action_103 (24) = happyShift action_38
action_103 (25) = happyShift action_39
action_103 (63) = happyShift action_70
action_103 (6) = happyGoto action_104
action_103 _ = happyReduce_6

action_104 _ = happyReduce_3

action_105 _ = happyReduce_14

action_106 _ = happyReduce_15

action_107 _ = happyReduce_44

action_108 _ = happyReduce_37

action_109 _ = happyReduce_36

action_110 _ = happyReduce_38

action_111 (14) = happyShift action_18
action_111 (16) = happyShift action_19
action_111 (18) = happyShift action_20
action_111 (26) = happyShift action_21
action_111 (28) = happyShift action_52
action_111 (29) = happyShift action_53
action_111 (30) = happyShift action_54
action_111 (31) = happyShift action_55
action_111 (32) = happyShift action_56
action_111 (33) = happyShift action_57
action_111 (34) = happyShift action_58
action_111 (35) = happyShift action_59
action_111 (36) = happyShift action_60
action_111 (37) = happyShift action_61
action_111 (38) = happyShift action_62
action_111 (39) = happyShift action_63
action_111 (40) = happyShift action_64
action_111 (41) = happyShift action_22
action_111 (42) = happyShift action_65
action_111 (43) = happyShift action_23
action_111 (44) = happyShift action_24
action_111 (45) = happyShift action_117
action_111 (46) = happyShift action_25
action_111 (49) = happyShift action_26
action_111 (50) = happyShift action_27
action_111 (51) = happyShift action_28
action_111 (52) = happyShift action_29
action_111 (53) = happyShift action_30
action_111 (54) = happyShift action_31
action_111 (55) = happyShift action_32
action_111 (56) = happyShift action_66
action_111 (64) = happyShift action_33
action_111 (65) = happyShift action_34
action_111 (66) = happyShift action_35
action_111 (67) = happyShift action_36
action_111 (9) = happyGoto action_50
action_111 (10) = happyGoto action_17
action_111 (12) = happyGoto action_51
action_111 _ = happyFail

action_112 (14) = happyShift action_18
action_112 (16) = happyShift action_19
action_112 (18) = happyShift action_20
action_112 (26) = happyShift action_21
action_112 (41) = happyShift action_22
action_112 (43) = happyShift action_23
action_112 (44) = happyShift action_24
action_112 (46) = happyShift action_25
action_112 (49) = happyShift action_26
action_112 (50) = happyShift action_27
action_112 (51) = happyShift action_28
action_112 (52) = happyShift action_29
action_112 (53) = happyShift action_30
action_112 (54) = happyShift action_31
action_112 (55) = happyShift action_32
action_112 (64) = happyShift action_33
action_112 (65) = happyShift action_34
action_112 (66) = happyShift action_35
action_112 (67) = happyShift action_36
action_112 (9) = happyGoto action_116
action_112 (10) = happyGoto action_17
action_112 _ = happyFail

action_113 (14) = happyShift action_18
action_113 (16) = happyShift action_19
action_113 (18) = happyShift action_20
action_113 (23) = happyShift action_4
action_113 (24) = happyShift action_38
action_113 (25) = happyShift action_39
action_113 (26) = happyShift action_21
action_113 (28) = happyShift action_52
action_113 (29) = happyShift action_53
action_113 (30) = happyShift action_54
action_113 (31) = happyShift action_55
action_113 (32) = happyShift action_56
action_113 (33) = happyShift action_57
action_113 (34) = happyShift action_58
action_113 (35) = happyShift action_59
action_113 (36) = happyShift action_60
action_113 (37) = happyShift action_61
action_113 (38) = happyShift action_62
action_113 (39) = happyShift action_63
action_113 (40) = happyShift action_64
action_113 (41) = happyShift action_22
action_113 (42) = happyShift action_65
action_113 (43) = happyShift action_23
action_113 (44) = happyShift action_24
action_113 (46) = happyShift action_25
action_113 (49) = happyShift action_26
action_113 (50) = happyShift action_27
action_113 (51) = happyShift action_28
action_113 (52) = happyShift action_29
action_113 (53) = happyShift action_30
action_113 (54) = happyShift action_31
action_113 (55) = happyShift action_32
action_113 (56) = happyShift action_66
action_113 (64) = happyShift action_33
action_113 (65) = happyShift action_34
action_113 (66) = happyShift action_35
action_113 (67) = happyShift action_36
action_113 (6) = happyGoto action_115
action_113 (9) = happyGoto action_50
action_113 (10) = happyGoto action_17
action_113 (12) = happyGoto action_51
action_113 _ = happyReduce_6

action_114 _ = happyReduce_4

action_115 _ = happyReduce_5

action_116 (26) = happyShift action_21
action_116 (28) = happyShift action_52
action_116 (29) = happyShift action_53
action_116 (30) = happyShift action_54
action_116 (31) = happyShift action_55
action_116 (32) = happyShift action_56
action_116 (33) = happyShift action_57
action_116 (34) = happyShift action_58
action_116 (35) = happyShift action_59
action_116 (36) = happyShift action_60
action_116 (37) = happyShift action_61
action_116 (38) = happyShift action_62
action_116 (39) = happyShift action_63
action_116 (40) = happyShift action_64
action_116 (42) = happyShift action_65
action_116 (44) = happyFail
action_116 (46) = happyFail
action_116 (56) = happyShift action_66
action_116 (64) = happyFail
action_116 (65) = happyFail
action_116 (66) = happyFail
action_116 (67) = happyShift action_36
action_116 (9) = happyGoto action_50
action_116 (10) = happyGoto action_17
action_116 (12) = happyGoto action_51
action_116 _ = happyReduce_23

action_117 (14) = happyShift action_18
action_117 (16) = happyShift action_19
action_117 (18) = happyShift action_20
action_117 (26) = happyShift action_21
action_117 (41) = happyShift action_22
action_117 (43) = happyShift action_23
action_117 (44) = happyShift action_24
action_117 (46) = happyShift action_25
action_117 (49) = happyShift action_26
action_117 (50) = happyShift action_27
action_117 (51) = happyShift action_28
action_117 (52) = happyShift action_29
action_117 (53) = happyShift action_30
action_117 (54) = happyShift action_31
action_117 (55) = happyShift action_32
action_117 (64) = happyShift action_33
action_117 (65) = happyShift action_34
action_117 (66) = happyShift action_35
action_117 (67) = happyShift action_36
action_117 (9) = happyGoto action_118
action_117 (10) = happyGoto action_17
action_117 _ = happyFail

action_118 (26) = happyShift action_21
action_118 (28) = happyShift action_52
action_118 (29) = happyShift action_53
action_118 (30) = happyShift action_54
action_118 (31) = happyShift action_55
action_118 (32) = happyShift action_56
action_118 (33) = happyShift action_57
action_118 (34) = happyShift action_58
action_118 (35) = happyShift action_59
action_118 (36) = happyShift action_60
action_118 (37) = happyShift action_61
action_118 (38) = happyShift action_62
action_118 (39) = happyShift action_63
action_118 (40) = happyShift action_64
action_118 (42) = happyShift action_65
action_118 (44) = happyFail
action_118 (46) = happyFail
action_118 (56) = happyShift action_66
action_118 (64) = happyFail
action_118 (65) = happyFail
action_118 (66) = happyFail
action_118 (67) = happyShift action_36
action_118 (9) = happyGoto action_50
action_118 (10) = happyGoto action_17
action_118 (12) = happyGoto action_51
action_118 _ = happyReduce_19

happyReduce_3 = happyReduce 5 6 happyReduction_3
happyReduction_3 ((HappyAbsSyn6  happy_var_5) `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (addAlias happy_var_5 (ParserTVar happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 5 6 happyReduction_4
happyReduction_4 ((HappyAbsSyn6  happy_var_5) `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdLC   p happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (addTyDec happy_var_5 (happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 6 6 happyReduction_5
happyReduction_5 ((HappyAbsSyn6  happy_var_6) `HappyStk`
	(HappyAbsSyn9  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	(HappyTerminal (TokenIdLC   p happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (addFuncPR happy_var_6 (FuncInf happy_var_2 happy_var_3 happy_var_5)
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_0  6 happyReduction_6
happyReduction_6  =  HappyAbsSyn6
		 (emptyPR
	)

happyReduce_7 = happySpecReduce_2  7 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal (TokenIdLC   p happy_var_1))
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_0  7 happyReduction_8
happyReduction_8  =  HappyAbsSyn7
		 ([]
	)

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn8
		 (TInt
	)

happyReduce_10 = happySpecReduce_1  8 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn8
		 (TBool
	)

happyReduce_11 = happySpecReduce_1  8 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn8
		 (TChar
	)

happyReduce_12 = happySpecReduce_1  8 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn8
		 (TList TChar
	)

happyReduce_13 = happySpecReduce_3  8 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (TList happy_var_2
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 5 8 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TSum happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 5 8 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (TProd happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_3  8 happyReduction_16
happyReduction_16 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (TFunc happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  8 happyReduction_17
happyReduction_17 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  8 happyReduction_18
happyReduction_18 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn8
		 (ParserTVar happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happyReduce 7 9 happyReduction_19
happyReduction_19 ((HappyAbsSyn9  happy_var_7) `HappyStk`
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

happyReduce_20 = happyReduce 4 9 happyReduction_20
happyReduction_20 ((HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdLC   p happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (AbsInf happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_21 = happySpecReduce_2  9 happyReduction_21
happyReduction_21 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (App happy_var_1 happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  9 happyReduction_22
happyReduction_22 (HappyTerminal (TokenIdLC   p happy_var_1))
	 =  HappyAbsSyn9
		 (Var happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happyReduce 6 9 happyReduction_23
happyReduction_23 ((HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (App (App (App (Operation Cond) happy_var_2) happy_var_4) happy_var_6
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_3  9 happyReduction_24
happyReduction_24 (HappyAbsSyn9  happy_var_3)
	(HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (App (App happy_var_2 happy_var_1) happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  9 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn9
		 (Operation IsZ
	)

happyReduce_26 = happySpecReduce_1  9 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn9
		 (Operation Not
	)

happyReduce_27 = happySpecReduce_1  9 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn9
		 (Operation RemL
	)

happyReduce_28 = happySpecReduce_1  9 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn9
		 (Operation RemR
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
	TokenAlias  p -> cont 23;
	TokenDef    p -> cont 24;
	TokenType   p -> cont 25;
	TokenLambda p -> cont 26;
	TokenDot    p -> cont 27;
	TokenAdd    p -> cont 28;
	TokenSub    p -> cont 29;
	TokenMul    p -> cont 30;
	TokenDiv    p -> cont 31;
	TokenMod    p -> cont 32;
	TokenEqEq   p -> cont 33;
	TokenLsEq   p -> cont 34;
	TokenLess   p -> cont 35;
	TokenGtEq   p -> cont 36;
	TokenGrtr   p -> cont 37;
	TokenNoEq   p -> cont 38;
	TokenAnd    p -> cont 39;
	TokenOr     p -> cont 40;
	TokenNot    p -> cont 41;
	TokenXor    p -> cont 42;
	TokenIsZero p -> cont 43;
	TokenLet    p -> cont 44;
	TokenIn     p -> cont 45;
	TokenIf     p -> cont 46;
	TokenThen   p -> cont 47;
	TokenElse   p -> cont 48;
	TokenRemL   p -> cont 49;
	TokenRemR   p -> cont 50;
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
