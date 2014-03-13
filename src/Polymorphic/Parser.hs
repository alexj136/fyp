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
	| HappyAbsSyn8 (Maybe Type)
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
 happyReduce_61 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (24) = happyShift action_4
action_0 (25) = happyShift action_36
action_0 (26) = happyShift action_37
action_0 (6) = happyGoto action_35
action_0 _ = happyReduce_6

action_1 (14) = happyShift action_16
action_1 (16) = happyShift action_17
action_1 (18) = happyShift action_18
action_1 (27) = happyShift action_19
action_1 (42) = happyShift action_20
action_1 (44) = happyShift action_21
action_1 (45) = happyShift action_22
action_1 (47) = happyShift action_23
action_1 (50) = happyShift action_24
action_1 (51) = happyShift action_25
action_1 (52) = happyShift action_26
action_1 (53) = happyShift action_27
action_1 (54) = happyShift action_28
action_1 (55) = happyShift action_29
action_1 (56) = happyShift action_30
action_1 (64) = happyShift action_31
action_1 (65) = happyShift action_32
action_1 (66) = happyShift action_33
action_1 (67) = happyShift action_34
action_1 (10) = happyGoto action_14
action_1 (11) = happyGoto action_15
action_1 _ = happyFail

action_2 (14) = happyShift action_6
action_2 (16) = happyShift action_7
action_2 (18) = happyShift action_8
action_2 (57) = happyShift action_9
action_2 (58) = happyShift action_10
action_2 (59) = happyShift action_11
action_2 (60) = happyShift action_12
action_2 (67) = happyShift action_13
action_2 (9) = happyGoto action_5
action_2 _ = happyFail

action_3 (24) = happyShift action_4
action_3 _ = happyFail

action_4 (67) = happyShift action_68
action_4 _ = happyFail

action_5 (63) = happyShift action_67
action_5 (69) = happyAccept
action_5 _ = happyFail

action_6 (14) = happyShift action_6
action_6 (16) = happyShift action_7
action_6 (18) = happyShift action_8
action_6 (57) = happyShift action_9
action_6 (58) = happyShift action_10
action_6 (59) = happyShift action_11
action_6 (60) = happyShift action_12
action_6 (67) = happyShift action_13
action_6 (9) = happyGoto action_66
action_6 _ = happyFail

action_7 (14) = happyShift action_6
action_7 (16) = happyShift action_7
action_7 (18) = happyShift action_8
action_7 (57) = happyShift action_9
action_7 (58) = happyShift action_10
action_7 (59) = happyShift action_11
action_7 (60) = happyShift action_12
action_7 (67) = happyShift action_13
action_7 (9) = happyGoto action_65
action_7 _ = happyFail

action_8 (14) = happyShift action_6
action_8 (16) = happyShift action_7
action_8 (18) = happyShift action_8
action_8 (57) = happyShift action_9
action_8 (58) = happyShift action_10
action_8 (59) = happyShift action_11
action_8 (60) = happyShift action_12
action_8 (67) = happyShift action_13
action_8 (9) = happyGoto action_64
action_8 _ = happyFail

action_9 _ = happyReduce_11

action_10 _ = happyReduce_12

action_11 _ = happyReduce_13

action_12 _ = happyReduce_14

action_13 _ = happyReduce_20

action_14 (14) = happyShift action_16
action_14 (16) = happyShift action_17
action_14 (18) = happyShift action_18
action_14 (27) = happyShift action_19
action_14 (29) = happyShift action_50
action_14 (30) = happyShift action_51
action_14 (31) = happyShift action_52
action_14 (32) = happyShift action_53
action_14 (33) = happyShift action_54
action_14 (34) = happyShift action_55
action_14 (35) = happyShift action_56
action_14 (36) = happyShift action_57
action_14 (37) = happyShift action_58
action_14 (38) = happyShift action_59
action_14 (39) = happyShift action_60
action_14 (40) = happyShift action_61
action_14 (41) = happyShift action_62
action_14 (42) = happyShift action_20
action_14 (43) = happyShift action_63
action_14 (44) = happyShift action_21
action_14 (45) = happyShift action_22
action_14 (47) = happyShift action_23
action_14 (50) = happyShift action_24
action_14 (51) = happyShift action_25
action_14 (52) = happyShift action_26
action_14 (53) = happyShift action_27
action_14 (54) = happyShift action_28
action_14 (55) = happyShift action_29
action_14 (56) = happyShift action_30
action_14 (64) = happyShift action_31
action_14 (65) = happyShift action_32
action_14 (66) = happyShift action_33
action_14 (67) = happyShift action_34
action_14 (69) = happyAccept
action_14 (10) = happyGoto action_48
action_14 (11) = happyGoto action_15
action_14 (13) = happyGoto action_49
action_14 _ = happyFail

action_15 _ = happyReduce_36

action_16 (14) = happyShift action_16
action_16 (16) = happyShift action_17
action_16 (18) = happyShift action_18
action_16 (27) = happyShift action_19
action_16 (42) = happyShift action_20
action_16 (44) = happyShift action_21
action_16 (45) = happyShift action_22
action_16 (47) = happyShift action_23
action_16 (50) = happyShift action_24
action_16 (51) = happyShift action_25
action_16 (52) = happyShift action_26
action_16 (53) = happyShift action_27
action_16 (54) = happyShift action_28
action_16 (55) = happyShift action_29
action_16 (56) = happyShift action_30
action_16 (64) = happyShift action_31
action_16 (65) = happyShift action_32
action_16 (66) = happyShift action_33
action_16 (67) = happyShift action_34
action_16 (10) = happyGoto action_47
action_16 (11) = happyGoto action_15
action_16 _ = happyFail

action_17 (14) = happyShift action_16
action_17 (16) = happyShift action_17
action_17 (17) = happyShift action_46
action_17 (18) = happyShift action_18
action_17 (27) = happyShift action_19
action_17 (42) = happyShift action_20
action_17 (44) = happyShift action_21
action_17 (45) = happyShift action_22
action_17 (47) = happyShift action_23
action_17 (50) = happyShift action_24
action_17 (51) = happyShift action_25
action_17 (52) = happyShift action_26
action_17 (53) = happyShift action_27
action_17 (54) = happyShift action_28
action_17 (55) = happyShift action_29
action_17 (56) = happyShift action_30
action_17 (64) = happyShift action_31
action_17 (65) = happyShift action_32
action_17 (66) = happyShift action_33
action_17 (67) = happyShift action_34
action_17 (10) = happyGoto action_45
action_17 (11) = happyGoto action_15
action_17 _ = happyFail

action_18 (14) = happyShift action_16
action_18 (16) = happyShift action_17
action_18 (18) = happyShift action_18
action_18 (23) = happyShift action_44
action_18 (27) = happyShift action_19
action_18 (42) = happyShift action_20
action_18 (44) = happyShift action_21
action_18 (45) = happyShift action_22
action_18 (47) = happyShift action_23
action_18 (50) = happyShift action_24
action_18 (51) = happyShift action_25
action_18 (52) = happyShift action_26
action_18 (53) = happyShift action_27
action_18 (54) = happyShift action_28
action_18 (55) = happyShift action_29
action_18 (56) = happyShift action_30
action_18 (64) = happyShift action_31
action_18 (65) = happyShift action_32
action_18 (66) = happyShift action_33
action_18 (67) = happyShift action_34
action_18 (10) = happyGoto action_43
action_18 (11) = happyGoto action_15
action_18 _ = happyFail

action_19 (67) = happyShift action_42
action_19 _ = happyFail

action_20 _ = happyReduce_28

action_21 _ = happyReduce_27

action_22 (67) = happyShift action_41
action_22 _ = happyFail

action_23 (14) = happyShift action_16
action_23 (16) = happyShift action_17
action_23 (18) = happyShift action_18
action_23 (27) = happyShift action_19
action_23 (42) = happyShift action_20
action_23 (44) = happyShift action_21
action_23 (45) = happyShift action_22
action_23 (47) = happyShift action_23
action_23 (50) = happyShift action_24
action_23 (51) = happyShift action_25
action_23 (52) = happyShift action_26
action_23 (53) = happyShift action_27
action_23 (54) = happyShift action_28
action_23 (55) = happyShift action_29
action_23 (56) = happyShift action_30
action_23 (64) = happyShift action_31
action_23 (65) = happyShift action_32
action_23 (66) = happyShift action_33
action_23 (67) = happyShift action_34
action_23 (10) = happyGoto action_40
action_23 (11) = happyGoto action_15
action_23 _ = happyFail

action_24 _ = happyReduce_29

action_25 _ = happyReduce_30

action_26 _ = happyReduce_31

action_27 _ = happyReduce_32

action_28 _ = happyReduce_33

action_29 _ = happyReduce_34

action_30 _ = happyReduce_35

action_31 _ = happyReduce_41

action_32 _ = happyReduce_42

action_33 _ = happyReduce_43

action_34 _ = happyReduce_24

action_35 (69) = happyAccept
action_35 _ = happyFail

action_36 (67) = happyShift action_39
action_36 _ = happyFail

action_37 (67) = happyShift action_38
action_37 _ = happyFail

action_38 (20) = happyShift action_87
action_38 _ = happyFail

action_39 (67) = happyShift action_86
action_39 (7) = happyGoto action_85
action_39 _ = happyReduce_8

action_40 (14) = happyShift action_16
action_40 (16) = happyShift action_17
action_40 (18) = happyShift action_18
action_40 (27) = happyShift action_19
action_40 (29) = happyShift action_50
action_40 (30) = happyShift action_51
action_40 (31) = happyShift action_52
action_40 (32) = happyShift action_53
action_40 (33) = happyShift action_54
action_40 (34) = happyShift action_55
action_40 (35) = happyShift action_56
action_40 (36) = happyShift action_57
action_40 (37) = happyShift action_58
action_40 (38) = happyShift action_59
action_40 (39) = happyShift action_60
action_40 (40) = happyShift action_61
action_40 (41) = happyShift action_62
action_40 (42) = happyShift action_20
action_40 (43) = happyShift action_63
action_40 (44) = happyShift action_21
action_40 (45) = happyShift action_22
action_40 (47) = happyShift action_23
action_40 (48) = happyShift action_84
action_40 (50) = happyShift action_24
action_40 (51) = happyShift action_25
action_40 (52) = happyShift action_26
action_40 (53) = happyShift action_27
action_40 (54) = happyShift action_28
action_40 (55) = happyShift action_29
action_40 (56) = happyShift action_30
action_40 (64) = happyShift action_31
action_40 (65) = happyShift action_32
action_40 (66) = happyShift action_33
action_40 (67) = happyShift action_34
action_40 (10) = happyGoto action_48
action_40 (11) = happyGoto action_15
action_40 (13) = happyGoto action_49
action_40 _ = happyFail

action_41 (20) = happyShift action_83
action_41 _ = happyFail

action_42 (28) = happyShift action_82
action_42 _ = happyFail

action_43 (14) = happyShift action_16
action_43 (16) = happyShift action_17
action_43 (18) = happyShift action_18
action_43 (21) = happyShift action_81
action_43 (27) = happyShift action_19
action_43 (29) = happyShift action_50
action_43 (30) = happyShift action_51
action_43 (31) = happyShift action_52
action_43 (32) = happyShift action_53
action_43 (33) = happyShift action_54
action_43 (34) = happyShift action_55
action_43 (35) = happyShift action_56
action_43 (36) = happyShift action_57
action_43 (37) = happyShift action_58
action_43 (38) = happyShift action_59
action_43 (39) = happyShift action_60
action_43 (40) = happyShift action_61
action_43 (41) = happyShift action_62
action_43 (42) = happyShift action_20
action_43 (43) = happyShift action_63
action_43 (44) = happyShift action_21
action_43 (45) = happyShift action_22
action_43 (47) = happyShift action_23
action_43 (50) = happyShift action_24
action_43 (51) = happyShift action_25
action_43 (52) = happyShift action_26
action_43 (53) = happyShift action_27
action_43 (54) = happyShift action_28
action_43 (55) = happyShift action_29
action_43 (56) = happyShift action_30
action_43 (64) = happyShift action_31
action_43 (65) = happyShift action_32
action_43 (66) = happyShift action_33
action_43 (67) = happyShift action_34
action_43 (10) = happyGoto action_48
action_43 (11) = happyGoto action_15
action_43 (13) = happyGoto action_49
action_43 _ = happyFail

action_44 (21) = happyShift action_80
action_44 _ = happyFail

action_45 (14) = happyShift action_16
action_45 (16) = happyShift action_17
action_45 (17) = happyShift action_78
action_45 (18) = happyShift action_18
action_45 (21) = happyShift action_79
action_45 (27) = happyShift action_19
action_45 (29) = happyShift action_50
action_45 (30) = happyShift action_51
action_45 (31) = happyShift action_52
action_45 (32) = happyShift action_53
action_45 (33) = happyShift action_54
action_45 (34) = happyShift action_55
action_45 (35) = happyShift action_56
action_45 (36) = happyShift action_57
action_45 (37) = happyShift action_58
action_45 (38) = happyShift action_59
action_45 (39) = happyShift action_60
action_45 (40) = happyShift action_61
action_45 (41) = happyShift action_62
action_45 (42) = happyShift action_20
action_45 (43) = happyShift action_63
action_45 (44) = happyShift action_21
action_45 (45) = happyShift action_22
action_45 (47) = happyShift action_23
action_45 (50) = happyShift action_24
action_45 (51) = happyShift action_25
action_45 (52) = happyShift action_26
action_45 (53) = happyShift action_27
action_45 (54) = happyShift action_28
action_45 (55) = happyShift action_29
action_45 (56) = happyShift action_30
action_45 (64) = happyShift action_31
action_45 (65) = happyShift action_32
action_45 (66) = happyShift action_33
action_45 (67) = happyShift action_34
action_45 (10) = happyGoto action_48
action_45 (11) = happyGoto action_15
action_45 (12) = happyGoto action_77
action_45 (13) = happyGoto action_49
action_45 _ = happyFail

action_46 _ = happyReduce_44

action_47 (14) = happyShift action_16
action_47 (15) = happyShift action_76
action_47 (16) = happyShift action_17
action_47 (18) = happyShift action_18
action_47 (27) = happyShift action_19
action_47 (29) = happyShift action_50
action_47 (30) = happyShift action_51
action_47 (31) = happyShift action_52
action_47 (32) = happyShift action_53
action_47 (33) = happyShift action_54
action_47 (34) = happyShift action_55
action_47 (35) = happyShift action_56
action_47 (36) = happyShift action_57
action_47 (37) = happyShift action_58
action_47 (38) = happyShift action_59
action_47 (39) = happyShift action_60
action_47 (40) = happyShift action_61
action_47 (41) = happyShift action_62
action_47 (42) = happyShift action_20
action_47 (43) = happyShift action_63
action_47 (44) = happyShift action_21
action_47 (45) = happyShift action_22
action_47 (47) = happyShift action_23
action_47 (50) = happyShift action_24
action_47 (51) = happyShift action_25
action_47 (52) = happyShift action_26
action_47 (53) = happyShift action_27
action_47 (54) = happyShift action_28
action_47 (55) = happyShift action_29
action_47 (56) = happyShift action_30
action_47 (64) = happyShift action_31
action_47 (65) = happyShift action_32
action_47 (66) = happyShift action_33
action_47 (67) = happyShift action_34
action_47 (10) = happyGoto action_48
action_47 (11) = happyGoto action_15
action_47 (13) = happyGoto action_49
action_47 _ = happyFail

action_48 (27) = happyShift action_19
action_48 (10) = happyGoto action_48
action_48 (11) = happyGoto action_15
action_48 (13) = happyGoto action_49
action_48 _ = happyReduce_23

action_49 (14) = happyShift action_16
action_49 (16) = happyShift action_17
action_49 (18) = happyShift action_18
action_49 (27) = happyShift action_19
action_49 (42) = happyShift action_20
action_49 (44) = happyShift action_21
action_49 (45) = happyShift action_22
action_49 (47) = happyShift action_23
action_49 (50) = happyShift action_24
action_49 (51) = happyShift action_25
action_49 (52) = happyShift action_26
action_49 (53) = happyShift action_27
action_49 (54) = happyShift action_28
action_49 (55) = happyShift action_29
action_49 (56) = happyShift action_30
action_49 (64) = happyShift action_31
action_49 (65) = happyShift action_32
action_49 (66) = happyShift action_33
action_49 (67) = happyShift action_34
action_49 (10) = happyGoto action_75
action_49 (11) = happyGoto action_15
action_49 _ = happyFail

action_50 _ = happyReduce_48

action_51 _ = happyReduce_49

action_52 _ = happyReduce_50

action_53 _ = happyReduce_51

action_54 _ = happyReduce_52

action_55 _ = happyReduce_58

action_56 _ = happyReduce_57

action_57 _ = happyReduce_56

action_58 _ = happyReduce_61

action_59 _ = happyReduce_60

action_60 _ = happyReduce_59

action_61 _ = happyReduce_53

action_62 _ = happyReduce_54

action_63 _ = happyReduce_55

action_64 (61) = happyShift action_73
action_64 (62) = happyShift action_74
action_64 (63) = happyShift action_67
action_64 _ = happyFail

action_65 (17) = happyShift action_72
action_65 (63) = happyShift action_67
action_65 _ = happyFail

action_66 (15) = happyShift action_71
action_66 (63) = happyShift action_67
action_66 _ = happyFail

action_67 (14) = happyShift action_6
action_67 (16) = happyShift action_7
action_67 (18) = happyShift action_8
action_67 (57) = happyShift action_9
action_67 (58) = happyShift action_10
action_67 (59) = happyShift action_11
action_67 (60) = happyShift action_12
action_67 (67) = happyShift action_13
action_67 (9) = happyGoto action_70
action_67 _ = happyFail

action_68 (20) = happyShift action_69
action_68 _ = happyFail

action_69 (14) = happyShift action_6
action_69 (16) = happyShift action_7
action_69 (18) = happyShift action_8
action_69 (57) = happyShift action_9
action_69 (58) = happyShift action_10
action_69 (59) = happyShift action_11
action_69 (60) = happyShift action_12
action_69 (67) = happyShift action_13
action_69 (9) = happyGoto action_100
action_69 _ = happyFail

action_70 (63) = happyShift action_67
action_70 _ = happyReduce_18

action_71 _ = happyReduce_19

action_72 _ = happyReduce_15

action_73 (14) = happyShift action_6
action_73 (16) = happyShift action_7
action_73 (18) = happyShift action_8
action_73 (57) = happyShift action_9
action_73 (58) = happyShift action_10
action_73 (59) = happyShift action_11
action_73 (60) = happyShift action_12
action_73 (67) = happyShift action_13
action_73 (9) = happyGoto action_99
action_73 _ = happyFail

action_74 (14) = happyShift action_6
action_74 (16) = happyShift action_7
action_74 (18) = happyShift action_8
action_74 (57) = happyShift action_9
action_74 (58) = happyShift action_10
action_74 (59) = happyShift action_11
action_74 (60) = happyShift action_12
action_74 (67) = happyShift action_13
action_74 (9) = happyGoto action_98
action_74 _ = happyFail

action_75 (14) = happyShift action_16
action_75 (16) = happyShift action_17
action_75 (18) = happyShift action_18
action_75 (27) = happyShift action_19
action_75 (29) = happyShift action_50
action_75 (30) = happyShift action_51
action_75 (31) = happyShift action_52
action_75 (32) = happyShift action_53
action_75 (33) = happyShift action_54
action_75 (34) = happyShift action_55
action_75 (35) = happyShift action_56
action_75 (36) = happyShift action_57
action_75 (37) = happyShift action_58
action_75 (38) = happyShift action_59
action_75 (39) = happyShift action_60
action_75 (40) = happyShift action_61
action_75 (41) = happyShift action_62
action_75 (42) = happyShift action_20
action_75 (43) = happyShift action_63
action_75 (44) = happyShift action_21
action_75 (45) = happyShift action_22
action_75 (47) = happyShift action_23
action_75 (50) = happyShift action_24
action_75 (51) = happyShift action_25
action_75 (52) = happyShift action_26
action_75 (53) = happyShift action_27
action_75 (54) = happyShift action_28
action_75 (55) = happyShift action_29
action_75 (56) = happyShift action_30
action_75 (64) = happyShift action_31
action_75 (65) = happyShift action_32
action_75 (66) = happyShift action_33
action_75 (67) = happyShift action_34
action_75 (10) = happyGoto action_48
action_75 (11) = happyGoto action_15
action_75 (13) = happyGoto action_49
action_75 _ = happyReduce_26

action_76 _ = happyReduce_37

action_77 _ = happyReduce_45

action_78 _ = happyReduce_47

action_79 (14) = happyShift action_16
action_79 (16) = happyShift action_17
action_79 (18) = happyShift action_18
action_79 (27) = happyShift action_19
action_79 (42) = happyShift action_20
action_79 (44) = happyShift action_21
action_79 (45) = happyShift action_22
action_79 (47) = happyShift action_23
action_79 (50) = happyShift action_24
action_79 (51) = happyShift action_25
action_79 (52) = happyShift action_26
action_79 (53) = happyShift action_27
action_79 (54) = happyShift action_28
action_79 (55) = happyShift action_29
action_79 (56) = happyShift action_30
action_79 (64) = happyShift action_31
action_79 (65) = happyShift action_32
action_79 (66) = happyShift action_33
action_79 (67) = happyShift action_34
action_79 (10) = happyGoto action_97
action_79 (11) = happyGoto action_15
action_79 _ = happyFail

action_80 (14) = happyShift action_16
action_80 (16) = happyShift action_17
action_80 (18) = happyShift action_18
action_80 (27) = happyShift action_19
action_80 (42) = happyShift action_20
action_80 (44) = happyShift action_21
action_80 (45) = happyShift action_22
action_80 (47) = happyShift action_23
action_80 (50) = happyShift action_24
action_80 (51) = happyShift action_25
action_80 (52) = happyShift action_26
action_80 (53) = happyShift action_27
action_80 (54) = happyShift action_28
action_80 (55) = happyShift action_29
action_80 (56) = happyShift action_30
action_80 (64) = happyShift action_31
action_80 (65) = happyShift action_32
action_80 (66) = happyShift action_33
action_80 (67) = happyShift action_34
action_80 (10) = happyGoto action_96
action_80 (11) = happyGoto action_15
action_80 _ = happyFail

action_81 (14) = happyShift action_16
action_81 (16) = happyShift action_17
action_81 (18) = happyShift action_18
action_81 (23) = happyShift action_95
action_81 (27) = happyShift action_19
action_81 (42) = happyShift action_20
action_81 (44) = happyShift action_21
action_81 (45) = happyShift action_22
action_81 (47) = happyShift action_23
action_81 (50) = happyShift action_24
action_81 (51) = happyShift action_25
action_81 (52) = happyShift action_26
action_81 (53) = happyShift action_27
action_81 (54) = happyShift action_28
action_81 (55) = happyShift action_29
action_81 (56) = happyShift action_30
action_81 (64) = happyShift action_31
action_81 (65) = happyShift action_32
action_81 (66) = happyShift action_33
action_81 (67) = happyShift action_34
action_81 (10) = happyGoto action_94
action_81 (11) = happyGoto action_15
action_81 _ = happyFail

action_82 (14) = happyShift action_16
action_82 (16) = happyShift action_17
action_82 (18) = happyShift action_18
action_82 (27) = happyShift action_19
action_82 (42) = happyShift action_20
action_82 (44) = happyShift action_21
action_82 (45) = happyShift action_22
action_82 (47) = happyShift action_23
action_82 (50) = happyShift action_24
action_82 (51) = happyShift action_25
action_82 (52) = happyShift action_26
action_82 (53) = happyShift action_27
action_82 (54) = happyShift action_28
action_82 (55) = happyShift action_29
action_82 (56) = happyShift action_30
action_82 (64) = happyShift action_31
action_82 (65) = happyShift action_32
action_82 (66) = happyShift action_33
action_82 (67) = happyShift action_34
action_82 (10) = happyGoto action_93
action_82 (11) = happyGoto action_15
action_82 _ = happyFail

action_83 (14) = happyShift action_16
action_83 (16) = happyShift action_17
action_83 (18) = happyShift action_18
action_83 (27) = happyShift action_19
action_83 (42) = happyShift action_20
action_83 (44) = happyShift action_21
action_83 (45) = happyShift action_22
action_83 (47) = happyShift action_23
action_83 (50) = happyShift action_24
action_83 (51) = happyShift action_25
action_83 (52) = happyShift action_26
action_83 (53) = happyShift action_27
action_83 (54) = happyShift action_28
action_83 (55) = happyShift action_29
action_83 (56) = happyShift action_30
action_83 (64) = happyShift action_31
action_83 (65) = happyShift action_32
action_83 (66) = happyShift action_33
action_83 (67) = happyShift action_34
action_83 (10) = happyGoto action_92
action_83 (11) = happyGoto action_15
action_83 _ = happyFail

action_84 (14) = happyShift action_16
action_84 (16) = happyShift action_17
action_84 (18) = happyShift action_18
action_84 (27) = happyShift action_19
action_84 (42) = happyShift action_20
action_84 (44) = happyShift action_21
action_84 (45) = happyShift action_22
action_84 (47) = happyShift action_23
action_84 (50) = happyShift action_24
action_84 (51) = happyShift action_25
action_84 (52) = happyShift action_26
action_84 (53) = happyShift action_27
action_84 (54) = happyShift action_28
action_84 (55) = happyShift action_29
action_84 (56) = happyShift action_30
action_84 (64) = happyShift action_31
action_84 (65) = happyShift action_32
action_84 (66) = happyShift action_33
action_84 (67) = happyShift action_34
action_84 (10) = happyGoto action_91
action_84 (11) = happyGoto action_15
action_84 _ = happyFail

action_85 (20) = happyShift action_90
action_85 _ = happyFail

action_86 (67) = happyShift action_86
action_86 (7) = happyGoto action_89
action_86 _ = happyReduce_8

action_87 (14) = happyShift action_6
action_87 (16) = happyShift action_7
action_87 (18) = happyShift action_8
action_87 (57) = happyShift action_9
action_87 (58) = happyShift action_10
action_87 (59) = happyShift action_11
action_87 (60) = happyShift action_12
action_87 (67) = happyShift action_13
action_87 (9) = happyGoto action_88
action_87 _ = happyFail

action_88 (24) = happyShift action_4
action_88 (25) = happyShift action_36
action_88 (26) = happyShift action_37
action_88 (63) = happyShift action_67
action_88 (6) = happyGoto action_111
action_88 _ = happyReduce_6

action_89 _ = happyReduce_7

action_90 (14) = happyShift action_16
action_90 (16) = happyShift action_17
action_90 (18) = happyShift action_18
action_90 (27) = happyShift action_19
action_90 (42) = happyShift action_20
action_90 (44) = happyShift action_21
action_90 (45) = happyShift action_22
action_90 (47) = happyShift action_23
action_90 (50) = happyShift action_24
action_90 (51) = happyShift action_25
action_90 (52) = happyShift action_26
action_90 (53) = happyShift action_27
action_90 (54) = happyShift action_28
action_90 (55) = happyShift action_29
action_90 (56) = happyShift action_30
action_90 (64) = happyShift action_31
action_90 (65) = happyShift action_32
action_90 (66) = happyShift action_33
action_90 (67) = happyShift action_34
action_90 (10) = happyGoto action_110
action_90 (11) = happyGoto action_15
action_90 _ = happyFail

action_91 (14) = happyShift action_16
action_91 (16) = happyShift action_17
action_91 (18) = happyShift action_18
action_91 (27) = happyShift action_19
action_91 (29) = happyShift action_50
action_91 (30) = happyShift action_51
action_91 (31) = happyShift action_52
action_91 (32) = happyShift action_53
action_91 (33) = happyShift action_54
action_91 (34) = happyShift action_55
action_91 (35) = happyShift action_56
action_91 (36) = happyShift action_57
action_91 (37) = happyShift action_58
action_91 (38) = happyShift action_59
action_91 (39) = happyShift action_60
action_91 (40) = happyShift action_61
action_91 (41) = happyShift action_62
action_91 (42) = happyShift action_20
action_91 (43) = happyShift action_63
action_91 (44) = happyShift action_21
action_91 (45) = happyShift action_22
action_91 (47) = happyShift action_23
action_91 (49) = happyShift action_109
action_91 (50) = happyShift action_24
action_91 (51) = happyShift action_25
action_91 (52) = happyShift action_26
action_91 (53) = happyShift action_27
action_91 (54) = happyShift action_28
action_91 (55) = happyShift action_29
action_91 (56) = happyShift action_30
action_91 (64) = happyShift action_31
action_91 (65) = happyShift action_32
action_91 (66) = happyShift action_33
action_91 (67) = happyShift action_34
action_91 (10) = happyGoto action_48
action_91 (11) = happyGoto action_15
action_91 (13) = happyGoto action_49
action_91 _ = happyFail

action_92 (14) = happyShift action_16
action_92 (16) = happyShift action_17
action_92 (18) = happyShift action_18
action_92 (27) = happyShift action_19
action_92 (29) = happyShift action_50
action_92 (30) = happyShift action_51
action_92 (31) = happyShift action_52
action_92 (32) = happyShift action_53
action_92 (33) = happyShift action_54
action_92 (34) = happyShift action_55
action_92 (35) = happyShift action_56
action_92 (36) = happyShift action_57
action_92 (37) = happyShift action_58
action_92 (38) = happyShift action_59
action_92 (39) = happyShift action_60
action_92 (40) = happyShift action_61
action_92 (41) = happyShift action_62
action_92 (42) = happyShift action_20
action_92 (43) = happyShift action_63
action_92 (44) = happyShift action_21
action_92 (45) = happyShift action_22
action_92 (46) = happyShift action_108
action_92 (47) = happyShift action_23
action_92 (50) = happyShift action_24
action_92 (51) = happyShift action_25
action_92 (52) = happyShift action_26
action_92 (53) = happyShift action_27
action_92 (54) = happyShift action_28
action_92 (55) = happyShift action_29
action_92 (56) = happyShift action_30
action_92 (64) = happyShift action_31
action_92 (65) = happyShift action_32
action_92 (66) = happyShift action_33
action_92 (67) = happyShift action_34
action_92 (10) = happyGoto action_48
action_92 (11) = happyGoto action_15
action_92 (13) = happyGoto action_49
action_92 _ = happyFail

action_93 (27) = happyShift action_19
action_93 (29) = happyShift action_50
action_93 (30) = happyShift action_51
action_93 (31) = happyShift action_52
action_93 (32) = happyShift action_53
action_93 (33) = happyShift action_54
action_93 (34) = happyShift action_55
action_93 (35) = happyShift action_56
action_93 (36) = happyShift action_57
action_93 (37) = happyShift action_58
action_93 (38) = happyShift action_59
action_93 (39) = happyShift action_60
action_93 (40) = happyShift action_61
action_93 (41) = happyShift action_62
action_93 (43) = happyShift action_63
action_93 (45) = happyFail
action_93 (47) = happyFail
action_93 (64) = happyFail
action_93 (65) = happyFail
action_93 (66) = happyFail
action_93 (67) = happyShift action_34
action_93 (10) = happyGoto action_48
action_93 (11) = happyGoto action_15
action_93 (13) = happyGoto action_49
action_93 _ = happyReduce_22

action_94 (14) = happyShift action_16
action_94 (16) = happyShift action_17
action_94 (18) = happyShift action_18
action_94 (19) = happyShift action_107
action_94 (27) = happyShift action_19
action_94 (29) = happyShift action_50
action_94 (30) = happyShift action_51
action_94 (31) = happyShift action_52
action_94 (32) = happyShift action_53
action_94 (33) = happyShift action_54
action_94 (34) = happyShift action_55
action_94 (35) = happyShift action_56
action_94 (36) = happyShift action_57
action_94 (37) = happyShift action_58
action_94 (38) = happyShift action_59
action_94 (39) = happyShift action_60
action_94 (40) = happyShift action_61
action_94 (41) = happyShift action_62
action_94 (42) = happyShift action_20
action_94 (43) = happyShift action_63
action_94 (44) = happyShift action_21
action_94 (45) = happyShift action_22
action_94 (47) = happyShift action_23
action_94 (50) = happyShift action_24
action_94 (51) = happyShift action_25
action_94 (52) = happyShift action_26
action_94 (53) = happyShift action_27
action_94 (54) = happyShift action_28
action_94 (55) = happyShift action_29
action_94 (56) = happyShift action_30
action_94 (64) = happyShift action_31
action_94 (65) = happyShift action_32
action_94 (66) = happyShift action_33
action_94 (67) = happyShift action_34
action_94 (10) = happyGoto action_48
action_94 (11) = happyGoto action_15
action_94 (13) = happyGoto action_49
action_94 _ = happyFail

action_95 (19) = happyShift action_106
action_95 _ = happyFail

action_96 (14) = happyShift action_16
action_96 (16) = happyShift action_17
action_96 (18) = happyShift action_18
action_96 (19) = happyShift action_105
action_96 (27) = happyShift action_19
action_96 (29) = happyShift action_50
action_96 (30) = happyShift action_51
action_96 (31) = happyShift action_52
action_96 (32) = happyShift action_53
action_96 (33) = happyShift action_54
action_96 (34) = happyShift action_55
action_96 (35) = happyShift action_56
action_96 (36) = happyShift action_57
action_96 (37) = happyShift action_58
action_96 (38) = happyShift action_59
action_96 (39) = happyShift action_60
action_96 (40) = happyShift action_61
action_96 (41) = happyShift action_62
action_96 (42) = happyShift action_20
action_96 (43) = happyShift action_63
action_96 (44) = happyShift action_21
action_96 (45) = happyShift action_22
action_96 (47) = happyShift action_23
action_96 (50) = happyShift action_24
action_96 (51) = happyShift action_25
action_96 (52) = happyShift action_26
action_96 (53) = happyShift action_27
action_96 (54) = happyShift action_28
action_96 (55) = happyShift action_29
action_96 (56) = happyShift action_30
action_96 (64) = happyShift action_31
action_96 (65) = happyShift action_32
action_96 (66) = happyShift action_33
action_96 (67) = happyShift action_34
action_96 (10) = happyGoto action_48
action_96 (11) = happyGoto action_15
action_96 (13) = happyGoto action_49
action_96 _ = happyFail

action_97 (14) = happyShift action_16
action_97 (16) = happyShift action_17
action_97 (17) = happyShift action_78
action_97 (18) = happyShift action_18
action_97 (21) = happyShift action_79
action_97 (27) = happyShift action_19
action_97 (29) = happyShift action_50
action_97 (30) = happyShift action_51
action_97 (31) = happyShift action_52
action_97 (32) = happyShift action_53
action_97 (33) = happyShift action_54
action_97 (34) = happyShift action_55
action_97 (35) = happyShift action_56
action_97 (36) = happyShift action_57
action_97 (37) = happyShift action_58
action_97 (38) = happyShift action_59
action_97 (39) = happyShift action_60
action_97 (40) = happyShift action_61
action_97 (41) = happyShift action_62
action_97 (42) = happyShift action_20
action_97 (43) = happyShift action_63
action_97 (44) = happyShift action_21
action_97 (45) = happyShift action_22
action_97 (47) = happyShift action_23
action_97 (50) = happyShift action_24
action_97 (51) = happyShift action_25
action_97 (52) = happyShift action_26
action_97 (53) = happyShift action_27
action_97 (54) = happyShift action_28
action_97 (55) = happyShift action_29
action_97 (56) = happyShift action_30
action_97 (64) = happyShift action_31
action_97 (65) = happyShift action_32
action_97 (66) = happyShift action_33
action_97 (67) = happyShift action_34
action_97 (10) = happyGoto action_48
action_97 (11) = happyGoto action_15
action_97 (12) = happyGoto action_104
action_97 (13) = happyGoto action_49
action_97 _ = happyFail

action_98 (19) = happyShift action_103
action_98 (63) = happyShift action_67
action_98 _ = happyFail

action_99 (19) = happyShift action_102
action_99 (63) = happyShift action_67
action_99 _ = happyFail

action_100 (24) = happyShift action_4
action_100 (25) = happyShift action_36
action_100 (26) = happyShift action_37
action_100 (63) = happyShift action_67
action_100 (6) = happyGoto action_101
action_100 _ = happyReduce_6

action_101 _ = happyReduce_3

action_102 _ = happyReduce_16

action_103 _ = happyReduce_17

action_104 _ = happyReduce_46

action_105 _ = happyReduce_39

action_106 _ = happyReduce_38

action_107 _ = happyReduce_40

action_108 (14) = happyShift action_16
action_108 (16) = happyShift action_17
action_108 (18) = happyShift action_18
action_108 (27) = happyShift action_19
action_108 (42) = happyShift action_20
action_108 (44) = happyShift action_21
action_108 (45) = happyShift action_22
action_108 (47) = happyShift action_23
action_108 (50) = happyShift action_24
action_108 (51) = happyShift action_25
action_108 (52) = happyShift action_26
action_108 (53) = happyShift action_27
action_108 (54) = happyShift action_28
action_108 (55) = happyShift action_29
action_108 (56) = happyShift action_30
action_108 (64) = happyShift action_31
action_108 (65) = happyShift action_32
action_108 (66) = happyShift action_33
action_108 (67) = happyShift action_34
action_108 (10) = happyGoto action_114
action_108 (11) = happyGoto action_15
action_108 _ = happyFail

action_109 (14) = happyShift action_16
action_109 (16) = happyShift action_17
action_109 (18) = happyShift action_18
action_109 (27) = happyShift action_19
action_109 (42) = happyShift action_20
action_109 (44) = happyShift action_21
action_109 (45) = happyShift action_22
action_109 (47) = happyShift action_23
action_109 (50) = happyShift action_24
action_109 (51) = happyShift action_25
action_109 (52) = happyShift action_26
action_109 (53) = happyShift action_27
action_109 (54) = happyShift action_28
action_109 (55) = happyShift action_29
action_109 (56) = happyShift action_30
action_109 (64) = happyShift action_31
action_109 (65) = happyShift action_32
action_109 (66) = happyShift action_33
action_109 (67) = happyShift action_34
action_109 (10) = happyGoto action_113
action_109 (11) = happyGoto action_15
action_109 _ = happyFail

action_110 (14) = happyShift action_16
action_110 (16) = happyShift action_17
action_110 (18) = happyShift action_18
action_110 (24) = happyShift action_4
action_110 (25) = happyShift action_36
action_110 (26) = happyShift action_37
action_110 (27) = happyShift action_19
action_110 (29) = happyShift action_50
action_110 (30) = happyShift action_51
action_110 (31) = happyShift action_52
action_110 (32) = happyShift action_53
action_110 (33) = happyShift action_54
action_110 (34) = happyShift action_55
action_110 (35) = happyShift action_56
action_110 (36) = happyShift action_57
action_110 (37) = happyShift action_58
action_110 (38) = happyShift action_59
action_110 (39) = happyShift action_60
action_110 (40) = happyShift action_61
action_110 (41) = happyShift action_62
action_110 (42) = happyShift action_20
action_110 (43) = happyShift action_63
action_110 (44) = happyShift action_21
action_110 (45) = happyShift action_22
action_110 (47) = happyShift action_23
action_110 (50) = happyShift action_24
action_110 (51) = happyShift action_25
action_110 (52) = happyShift action_26
action_110 (53) = happyShift action_27
action_110 (54) = happyShift action_28
action_110 (55) = happyShift action_29
action_110 (56) = happyShift action_30
action_110 (64) = happyShift action_31
action_110 (65) = happyShift action_32
action_110 (66) = happyShift action_33
action_110 (67) = happyShift action_34
action_110 (6) = happyGoto action_112
action_110 (10) = happyGoto action_48
action_110 (11) = happyGoto action_15
action_110 (13) = happyGoto action_49
action_110 _ = happyReduce_6

action_111 _ = happyReduce_4

action_112 _ = happyReduce_5

action_113 (27) = happyShift action_19
action_113 (29) = happyShift action_50
action_113 (30) = happyShift action_51
action_113 (31) = happyShift action_52
action_113 (32) = happyShift action_53
action_113 (33) = happyShift action_54
action_113 (34) = happyShift action_55
action_113 (35) = happyShift action_56
action_113 (36) = happyShift action_57
action_113 (37) = happyShift action_58
action_113 (38) = happyShift action_59
action_113 (39) = happyShift action_60
action_113 (40) = happyShift action_61
action_113 (41) = happyShift action_62
action_113 (43) = happyShift action_63
action_113 (45) = happyFail
action_113 (47) = happyFail
action_113 (64) = happyFail
action_113 (65) = happyFail
action_113 (66) = happyFail
action_113 (67) = happyShift action_34
action_113 (10) = happyGoto action_48
action_113 (11) = happyGoto action_15
action_113 (13) = happyGoto action_49
action_113 _ = happyReduce_25

action_114 (27) = happyShift action_19
action_114 (29) = happyShift action_50
action_114 (30) = happyShift action_51
action_114 (31) = happyShift action_52
action_114 (32) = happyShift action_53
action_114 (33) = happyShift action_54
action_114 (34) = happyShift action_55
action_114 (35) = happyShift action_56
action_114 (36) = happyShift action_57
action_114 (37) = happyShift action_58
action_114 (38) = happyShift action_59
action_114 (39) = happyShift action_60
action_114 (40) = happyShift action_61
action_114 (41) = happyShift action_62
action_114 (43) = happyShift action_63
action_114 (45) = happyFail
action_114 (47) = happyFail
action_114 (64) = happyFail
action_114 (65) = happyFail
action_114 (66) = happyFail
action_114 (67) = happyShift action_34
action_114 (10) = happyGoto action_48
action_114 (11) = happyGoto action_15
action_114 (13) = happyGoto action_49
action_114 _ = happyReduce_21

happyReduce_3 = happyReduce 5 6 happyReduction_3
happyReduction_3 ((HappyAbsSyn6  happy_var_5) `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdLC   p happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (addAlias  (ParserTVar happy_var_2, happy_var_4) happy_var_5
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 5 6 happyReduction_4
happyReduction_4 ((HappyAbsSyn6  happy_var_5) `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdLC   p happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (addTyDec  (happy_var_2,            happy_var_4) happy_var_5
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 6 6 happyReduction_5
happyReduction_5 ((HappyAbsSyn6  happy_var_6) `HappyStk`
	(HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	(HappyTerminal (TokenIdLC   p happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (addFuncPR (makeFunc happy_var_2 happy_var_3 happy_var_5) happy_var_6
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

happyReduce_9 = happySpecReduce_2  8 happyReduction_9
happyReduction_9 _
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (Just happy_var_1
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_0  8 happyReduction_10
happyReduction_10  =  HappyAbsSyn8
		 (Nothing
	)

happyReduce_11 = happySpecReduce_1  9 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn9
		 (TInt
	)

happyReduce_12 = happySpecReduce_1  9 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn9
		 (TBool
	)

happyReduce_13 = happySpecReduce_1  9 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn9
		 (TChar
	)

happyReduce_14 = happySpecReduce_1  9 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn9
		 (TList TChar
	)

happyReduce_15 = happySpecReduce_3  9 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (TList happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 5 9 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (TSum happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 5 9 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (TProd happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_3  9 happyReduction_18
happyReduction_18 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (TFunc happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  9 happyReduction_19
happyReduction_19 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  9 happyReduction_20
happyReduction_20 (HappyTerminal (TokenIdLC   p happy_var_1))
	 =  HappyAbsSyn9
		 (ParserTVar happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happyReduce 6 10 happyReduction_21
happyReduction_21 ((HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdLC   p happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (App (AbsInf happy_var_2 happy_var_6) happy_var_4
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 4 10 happyReduction_22
happyReduction_22 ((HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdLC   p happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (AbsInf happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_2  10 happyReduction_23
happyReduction_23 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (App happy_var_1 happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  10 happyReduction_24
happyReduction_24 (HappyTerminal (TokenIdLC   p happy_var_1))
	 =  HappyAbsSyn10
		 (Var happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happyReduce 6 10 happyReduction_25
happyReduction_25 ((HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (App (App (App (Operation Cond) happy_var_2) happy_var_4) happy_var_6
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_3  10 happyReduction_26
happyReduction_26 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (App (App happy_var_2 happy_var_1) happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  10 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn10
		 (Operation IsZ
	)

happyReduce_28 = happySpecReduce_1  10 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn10
		 (Operation Not
	)

happyReduce_29 = happySpecReduce_1  10 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn10
		 (Operation RemL
	)

happyReduce_30 = happySpecReduce_1  10 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn10
		 (Operation RemR
	)

happyReduce_31 = happySpecReduce_1  10 happyReduction_31
happyReduction_31 _
	 =  HappyAbsSyn10
		 (Operation Fst
	)

happyReduce_32 = happySpecReduce_1  10 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn10
		 (Operation Snd
	)

happyReduce_33 = happySpecReduce_1  10 happyReduction_33
happyReduction_33 _
	 =  HappyAbsSyn10
		 (Operation Head
	)

happyReduce_34 = happySpecReduce_1  10 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn10
		 (Operation Tail
	)

happyReduce_35 = happySpecReduce_1  10 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn10
		 (Operation Null
	)

happyReduce_36 = happySpecReduce_1  10 happyReduction_36
happyReduction_36 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  10 happyReduction_37
happyReduction_37 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happyReduce 5 10 happyReduction_38
happyReduction_38 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (App (Operation InjL) happy_var_2
	) `HappyStk` happyRest

happyReduce_39 = happyReduce 5 10 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (App (Operation InjR) happy_var_4
	) `HappyStk` happyRest

happyReduce_40 = happyReduce 5 10 happyReduction_40
happyReduction_40 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (App (App (Operation Tuple) happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_41 = happySpecReduce_1  10 happyReduction_41
happyReduction_41 (HappyTerminal (TokenInt    p happy_var_1))
	 =  HappyAbsSyn10
		 (Constant (IntVal happy_var_1)
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  10 happyReduction_42
happyReduction_42 (HappyTerminal (TokenBool   p happy_var_1))
	 =  HappyAbsSyn10
		 (Constant (BoolVal happy_var_1)
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  10 happyReduction_43
happyReduction_43 (HappyTerminal (TokenStr    p happy_var_1))
	 =  HappyAbsSyn10
		 (parseStr happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_2  11 happyReduction_44
happyReduction_44 _
	_
	 =  HappyAbsSyn10
		 (Operation Empty
	)

happyReduce_45 = happySpecReduce_3  11 happyReduction_45
happyReduction_45 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (App (App (Operation Cons) happy_var_2) happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  12 happyReduction_46
happyReduction_46 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (App (App (Operation Cons) happy_var_2) happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  12 happyReduction_47
happyReduction_47 _
	 =  HappyAbsSyn10
		 (Operation Empty
	)

happyReduce_48 = happySpecReduce_1  13 happyReduction_48
happyReduction_48 _
	 =  HappyAbsSyn10
		 (Operation Add
	)

happyReduce_49 = happySpecReduce_1  13 happyReduction_49
happyReduction_49 _
	 =  HappyAbsSyn10
		 (Operation Sub
	)

happyReduce_50 = happySpecReduce_1  13 happyReduction_50
happyReduction_50 _
	 =  HappyAbsSyn10
		 (Operation Mul
	)

happyReduce_51 = happySpecReduce_1  13 happyReduction_51
happyReduction_51 _
	 =  HappyAbsSyn10
		 (Operation Div
	)

happyReduce_52 = happySpecReduce_1  13 happyReduction_52
happyReduction_52 _
	 =  HappyAbsSyn10
		 (Operation Mod
	)

happyReduce_53 = happySpecReduce_1  13 happyReduction_53
happyReduction_53 _
	 =  HappyAbsSyn10
		 (Operation And
	)

happyReduce_54 = happySpecReduce_1  13 happyReduction_54
happyReduction_54 _
	 =  HappyAbsSyn10
		 (Operation Or
	)

happyReduce_55 = happySpecReduce_1  13 happyReduction_55
happyReduction_55 _
	 =  HappyAbsSyn10
		 (Operation Xor
	)

happyReduce_56 = happySpecReduce_1  13 happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn10
		 (Operation Lss
	)

happyReduce_57 = happySpecReduce_1  13 happyReduction_57
happyReduction_57 _
	 =  HappyAbsSyn10
		 (Operation LsE
	)

happyReduce_58 = happySpecReduce_1  13 happyReduction_58
happyReduction_58 _
	 =  HappyAbsSyn10
		 (Operation Equ
	)

happyReduce_59 = happySpecReduce_1  13 happyReduction_59
happyReduction_59 _
	 =  HappyAbsSyn10
		 (Operation NEq
	)

happyReduce_60 = happySpecReduce_1  13 happyReduction_60
happyReduction_60 _
	 =  HappyAbsSyn10
		 (Operation Gtr
	)

happyReduce_61 = happySpecReduce_1  13 happyReduction_61
happyReduction_61 _
	 =  HappyAbsSyn10
		 (Operation GtE
	)

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
	TokenColon  p -> cont 22;
	TokenUndrSc p -> cont 23;
	TokenAlias  p -> cont 24;
	TokenDef    p -> cont 25;
	TokenType   p -> cont 26;
	TokenLambda p -> cont 27;
	TokenDot    p -> cont 28;
	TokenAdd    p -> cont 29;
	TokenSub    p -> cont 30;
	TokenMul    p -> cont 31;
	TokenDiv    p -> cont 32;
	TokenMod    p -> cont 33;
	TokenEqEq   p -> cont 34;
	TokenLsEq   p -> cont 35;
	TokenLess   p -> cont 36;
	TokenGtEq   p -> cont 37;
	TokenGrtr   p -> cont 38;
	TokenNoEq   p -> cont 39;
	TokenAnd    p -> cont 40;
	TokenOr     p -> cont 41;
	TokenNot    p -> cont 42;
	TokenXor    p -> cont 43;
	TokenIsZero p -> cont 44;
	TokenLet    p -> cont 45;
	TokenIn     p -> cont 46;
	TokenIf     p -> cont 47;
	TokenThen   p -> cont 48;
	TokenElse   p -> cont 49;
	TokenRemL   p -> cont 50;
	TokenRemR   p -> cont 51;
	TokenFst    p -> cont 52;
	TokenSnd    p -> cont 53;
	TokenHead   p -> cont 54;
	TokenTail   p -> cont 55;
	TokenNull   p -> cont 56;
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
