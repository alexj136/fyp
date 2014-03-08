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
 action_112 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
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
 happyReduce_56 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (14) = happyShift action_6
action_0 (16) = happyShift action_7
action_0 (18) = happyShift action_8
action_0 (51) = happyShift action_9
action_0 (52) = happyShift action_10
action_0 (53) = happyShift action_11
action_0 (54) = happyShift action_12
action_0 (61) = happyShift action_13
action_0 (6) = happyGoto action_33
action_0 (7) = happyGoto action_34
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
action_1 (58) = happyShift action_29
action_1 (59) = happyShift action_30
action_1 (60) = happyShift action_31
action_1 (61) = happyShift action_32
action_1 (10) = happyGoto action_15
action_1 (11) = happyGoto action_16
action_1 _ = happyFail

action_2 (14) = happyShift action_6
action_2 (16) = happyShift action_7
action_2 (18) = happyShift action_8
action_2 (51) = happyShift action_9
action_2 (52) = happyShift action_10
action_2 (53) = happyShift action_11
action_2 (54) = happyShift action_12
action_2 (61) = happyShift action_13
action_2 (9) = happyGoto action_14
action_2 _ = happyFail

action_3 (14) = happyShift action_6
action_3 (16) = happyShift action_7
action_3 (18) = happyShift action_8
action_3 (51) = happyShift action_9
action_3 (52) = happyShift action_10
action_3 (53) = happyShift action_11
action_3 (54) = happyShift action_12
action_3 (61) = happyShift action_13
action_3 (7) = happyGoto action_4
action_3 (9) = happyGoto action_5
action_3 _ = happyFail

action_4 (61) = happyShift action_65
action_4 _ = happyFail

action_5 (22) = happyShift action_64
action_5 (57) = happyShift action_60
action_5 _ = happyFail

action_6 (14) = happyShift action_6
action_6 (16) = happyShift action_7
action_6 (18) = happyShift action_8
action_6 (51) = happyShift action_9
action_6 (52) = happyShift action_10
action_6 (53) = happyShift action_11
action_6 (54) = happyShift action_12
action_6 (61) = happyShift action_13
action_6 (9) = happyGoto action_63
action_6 _ = happyFail

action_7 (14) = happyShift action_6
action_7 (16) = happyShift action_7
action_7 (18) = happyShift action_8
action_7 (51) = happyShift action_9
action_7 (52) = happyShift action_10
action_7 (53) = happyShift action_11
action_7 (54) = happyShift action_12
action_7 (61) = happyShift action_13
action_7 (9) = happyGoto action_62
action_7 _ = happyFail

action_8 (14) = happyShift action_6
action_8 (16) = happyShift action_7
action_8 (18) = happyShift action_8
action_8 (51) = happyShift action_9
action_8 (52) = happyShift action_10
action_8 (53) = happyShift action_11
action_8 (54) = happyShift action_12
action_8 (61) = happyShift action_13
action_8 (9) = happyGoto action_61
action_8 _ = happyFail

action_9 _ = happyReduce_9

action_10 _ = happyReduce_10

action_11 _ = happyReduce_11

action_12 _ = happyReduce_12

action_13 _ = happyReduce_18

action_14 (57) = happyShift action_60
action_14 (63) = happyAccept
action_14 _ = happyFail

action_15 (14) = happyShift action_17
action_15 (16) = happyShift action_18
action_15 (18) = happyShift action_19
action_15 (24) = happyShift action_20
action_15 (26) = happyShift action_46
action_15 (27) = happyShift action_47
action_15 (28) = happyShift action_48
action_15 (29) = happyShift action_49
action_15 (30) = happyShift action_50
action_15 (31) = happyShift action_51
action_15 (32) = happyShift action_52
action_15 (33) = happyShift action_53
action_15 (34) = happyShift action_54
action_15 (35) = happyShift action_55
action_15 (36) = happyShift action_56
action_15 (37) = happyShift action_57
action_15 (38) = happyShift action_58
action_15 (39) = happyShift action_21
action_15 (40) = happyShift action_59
action_15 (41) = happyShift action_22
action_15 (42) = happyShift action_23
action_15 (44) = happyShift action_24
action_15 (47) = happyShift action_25
action_15 (48) = happyShift action_26
action_15 (49) = happyShift action_27
action_15 (50) = happyShift action_28
action_15 (58) = happyShift action_29
action_15 (59) = happyShift action_30
action_15 (60) = happyShift action_31
action_15 (61) = happyShift action_32
action_15 (63) = happyAccept
action_15 (10) = happyGoto action_44
action_15 (11) = happyGoto action_16
action_15 (13) = happyGoto action_45
action_15 _ = happyFail

action_16 _ = happyReduce_31

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
action_17 (58) = happyShift action_29
action_17 (59) = happyShift action_30
action_17 (60) = happyShift action_31
action_17 (61) = happyShift action_32
action_17 (10) = happyGoto action_43
action_17 (11) = happyGoto action_16
action_17 _ = happyFail

action_18 (14) = happyShift action_17
action_18 (16) = happyShift action_18
action_18 (17) = happyShift action_42
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
action_18 (58) = happyShift action_29
action_18 (59) = happyShift action_30
action_18 (60) = happyShift action_31
action_18 (61) = happyShift action_32
action_18 (10) = happyGoto action_41
action_18 (11) = happyGoto action_16
action_18 _ = happyFail

action_19 (14) = happyShift action_17
action_19 (16) = happyShift action_18
action_19 (18) = happyShift action_19
action_19 (23) = happyShift action_40
action_19 (24) = happyShift action_20
action_19 (39) = happyShift action_21
action_19 (41) = happyShift action_22
action_19 (42) = happyShift action_23
action_19 (44) = happyShift action_24
action_19 (47) = happyShift action_25
action_19 (48) = happyShift action_26
action_19 (49) = happyShift action_27
action_19 (50) = happyShift action_28
action_19 (58) = happyShift action_29
action_19 (59) = happyShift action_30
action_19 (60) = happyShift action_31
action_19 (61) = happyShift action_32
action_19 (10) = happyGoto action_39
action_19 (11) = happyGoto action_16
action_19 _ = happyFail

action_20 (61) = happyShift action_38
action_20 _ = happyFail

action_21 _ = happyReduce_26

action_22 _ = happyReduce_25

action_23 (61) = happyShift action_37
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
action_24 (58) = happyShift action_29
action_24 (59) = happyShift action_30
action_24 (60) = happyShift action_31
action_24 (61) = happyShift action_32
action_24 (10) = happyGoto action_36
action_24 (11) = happyGoto action_16
action_24 _ = happyFail

action_25 _ = happyReduce_27

action_26 _ = happyReduce_28

action_27 _ = happyReduce_29

action_28 _ = happyReduce_30

action_29 _ = happyReduce_36

action_30 _ = happyReduce_37

action_31 _ = happyReduce_38

action_32 _ = happyReduce_22

action_33 (63) = happyAccept
action_33 _ = happyFail

action_34 (61) = happyShift action_35
action_34 _ = happyFail

action_35 (61) = happyShift action_67
action_35 (8) = happyGoto action_83
action_35 _ = happyReduce_8

action_36 (14) = happyShift action_17
action_36 (16) = happyShift action_18
action_36 (18) = happyShift action_19
action_36 (24) = happyShift action_20
action_36 (26) = happyShift action_46
action_36 (27) = happyShift action_47
action_36 (28) = happyShift action_48
action_36 (29) = happyShift action_49
action_36 (30) = happyShift action_50
action_36 (31) = happyShift action_51
action_36 (32) = happyShift action_52
action_36 (33) = happyShift action_53
action_36 (34) = happyShift action_54
action_36 (35) = happyShift action_55
action_36 (36) = happyShift action_56
action_36 (37) = happyShift action_57
action_36 (38) = happyShift action_58
action_36 (39) = happyShift action_21
action_36 (40) = happyShift action_59
action_36 (41) = happyShift action_22
action_36 (42) = happyShift action_23
action_36 (44) = happyShift action_24
action_36 (45) = happyShift action_82
action_36 (47) = happyShift action_25
action_36 (48) = happyShift action_26
action_36 (49) = happyShift action_27
action_36 (50) = happyShift action_28
action_36 (58) = happyShift action_29
action_36 (59) = happyShift action_30
action_36 (60) = happyShift action_31
action_36 (61) = happyShift action_32
action_36 (10) = happyGoto action_44
action_36 (11) = happyGoto action_16
action_36 (13) = happyGoto action_45
action_36 _ = happyFail

action_37 (20) = happyShift action_81
action_37 _ = happyFail

action_38 (25) = happyShift action_80
action_38 _ = happyFail

action_39 (14) = happyShift action_17
action_39 (16) = happyShift action_18
action_39 (18) = happyShift action_19
action_39 (21) = happyShift action_79
action_39 (24) = happyShift action_20
action_39 (26) = happyShift action_46
action_39 (27) = happyShift action_47
action_39 (28) = happyShift action_48
action_39 (29) = happyShift action_49
action_39 (30) = happyShift action_50
action_39 (31) = happyShift action_51
action_39 (32) = happyShift action_52
action_39 (33) = happyShift action_53
action_39 (34) = happyShift action_54
action_39 (35) = happyShift action_55
action_39 (36) = happyShift action_56
action_39 (37) = happyShift action_57
action_39 (38) = happyShift action_58
action_39 (39) = happyShift action_21
action_39 (40) = happyShift action_59
action_39 (41) = happyShift action_22
action_39 (42) = happyShift action_23
action_39 (44) = happyShift action_24
action_39 (47) = happyShift action_25
action_39 (48) = happyShift action_26
action_39 (49) = happyShift action_27
action_39 (50) = happyShift action_28
action_39 (58) = happyShift action_29
action_39 (59) = happyShift action_30
action_39 (60) = happyShift action_31
action_39 (61) = happyShift action_32
action_39 (10) = happyGoto action_44
action_39 (11) = happyGoto action_16
action_39 (13) = happyGoto action_45
action_39 _ = happyFail

action_40 (21) = happyShift action_78
action_40 _ = happyFail

action_41 (14) = happyShift action_17
action_41 (16) = happyShift action_18
action_41 (17) = happyShift action_76
action_41 (18) = happyShift action_19
action_41 (21) = happyShift action_77
action_41 (24) = happyShift action_20
action_41 (26) = happyShift action_46
action_41 (27) = happyShift action_47
action_41 (28) = happyShift action_48
action_41 (29) = happyShift action_49
action_41 (30) = happyShift action_50
action_41 (31) = happyShift action_51
action_41 (32) = happyShift action_52
action_41 (33) = happyShift action_53
action_41 (34) = happyShift action_54
action_41 (35) = happyShift action_55
action_41 (36) = happyShift action_56
action_41 (37) = happyShift action_57
action_41 (38) = happyShift action_58
action_41 (39) = happyShift action_21
action_41 (40) = happyShift action_59
action_41 (41) = happyShift action_22
action_41 (42) = happyShift action_23
action_41 (44) = happyShift action_24
action_41 (47) = happyShift action_25
action_41 (48) = happyShift action_26
action_41 (49) = happyShift action_27
action_41 (50) = happyShift action_28
action_41 (58) = happyShift action_29
action_41 (59) = happyShift action_30
action_41 (60) = happyShift action_31
action_41 (61) = happyShift action_32
action_41 (10) = happyGoto action_44
action_41 (11) = happyGoto action_16
action_41 (12) = happyGoto action_75
action_41 (13) = happyGoto action_45
action_41 _ = happyFail

action_42 _ = happyReduce_39

action_43 (14) = happyShift action_17
action_43 (15) = happyShift action_74
action_43 (16) = happyShift action_18
action_43 (18) = happyShift action_19
action_43 (24) = happyShift action_20
action_43 (26) = happyShift action_46
action_43 (27) = happyShift action_47
action_43 (28) = happyShift action_48
action_43 (29) = happyShift action_49
action_43 (30) = happyShift action_50
action_43 (31) = happyShift action_51
action_43 (32) = happyShift action_52
action_43 (33) = happyShift action_53
action_43 (34) = happyShift action_54
action_43 (35) = happyShift action_55
action_43 (36) = happyShift action_56
action_43 (37) = happyShift action_57
action_43 (38) = happyShift action_58
action_43 (39) = happyShift action_21
action_43 (40) = happyShift action_59
action_43 (41) = happyShift action_22
action_43 (42) = happyShift action_23
action_43 (44) = happyShift action_24
action_43 (47) = happyShift action_25
action_43 (48) = happyShift action_26
action_43 (49) = happyShift action_27
action_43 (50) = happyShift action_28
action_43 (58) = happyShift action_29
action_43 (59) = happyShift action_30
action_43 (60) = happyShift action_31
action_43 (61) = happyShift action_32
action_43 (10) = happyGoto action_44
action_43 (11) = happyGoto action_16
action_43 (13) = happyGoto action_45
action_43 _ = happyFail

action_44 (24) = happyShift action_20
action_44 (39) = happyShift action_21
action_44 (40) = happyShift action_59
action_44 (41) = happyShift action_22
action_44 (44) = happyShift action_24
action_44 (47) = happyShift action_25
action_44 (48) = happyShift action_26
action_44 (49) = happyShift action_27
action_44 (50) = happyShift action_28
action_44 (60) = happyShift action_31
action_44 (10) = happyGoto action_44
action_44 (11) = happyGoto action_16
action_44 (13) = happyGoto action_45
action_44 _ = happyReduce_21

action_45 (14) = happyShift action_17
action_45 (16) = happyShift action_18
action_45 (18) = happyShift action_19
action_45 (24) = happyShift action_20
action_45 (39) = happyShift action_21
action_45 (41) = happyShift action_22
action_45 (42) = happyShift action_23
action_45 (44) = happyShift action_24
action_45 (47) = happyShift action_25
action_45 (48) = happyShift action_26
action_45 (49) = happyShift action_27
action_45 (50) = happyShift action_28
action_45 (58) = happyShift action_29
action_45 (59) = happyShift action_30
action_45 (60) = happyShift action_31
action_45 (61) = happyShift action_32
action_45 (10) = happyGoto action_73
action_45 (11) = happyGoto action_16
action_45 _ = happyFail

action_46 _ = happyReduce_43

action_47 _ = happyReduce_44

action_48 _ = happyReduce_45

action_49 _ = happyReduce_46

action_50 _ = happyReduce_47

action_51 _ = happyReduce_53

action_52 _ = happyReduce_52

action_53 _ = happyReduce_51

action_54 _ = happyReduce_56

action_55 _ = happyReduce_55

action_56 _ = happyReduce_54

action_57 _ = happyReduce_48

action_58 _ = happyReduce_49

action_59 _ = happyReduce_50

action_60 (14) = happyShift action_6
action_60 (16) = happyShift action_7
action_60 (18) = happyShift action_8
action_60 (51) = happyShift action_9
action_60 (52) = happyShift action_10
action_60 (53) = happyShift action_11
action_60 (54) = happyShift action_12
action_60 (61) = happyShift action_13
action_60 (9) = happyGoto action_72
action_60 _ = happyFail

action_61 (55) = happyShift action_70
action_61 (56) = happyShift action_71
action_61 (57) = happyShift action_60
action_61 _ = happyFail

action_62 (17) = happyShift action_69
action_62 (57) = happyShift action_60
action_62 _ = happyFail

action_63 (15) = happyShift action_68
action_63 (57) = happyShift action_60
action_63 _ = happyFail

action_64 _ = happyReduce_5

action_65 (61) = happyShift action_67
action_65 (8) = happyGoto action_66
action_65 _ = happyFail

action_66 (20) = happyShift action_95
action_66 _ = happyFail

action_67 (61) = happyShift action_67
action_67 (8) = happyGoto action_94
action_67 _ = happyReduce_8

action_68 _ = happyReduce_17

action_69 _ = happyReduce_13

action_70 (14) = happyShift action_6
action_70 (16) = happyShift action_7
action_70 (18) = happyShift action_8
action_70 (51) = happyShift action_9
action_70 (52) = happyShift action_10
action_70 (53) = happyShift action_11
action_70 (54) = happyShift action_12
action_70 (61) = happyShift action_13
action_70 (9) = happyGoto action_93
action_70 _ = happyFail

action_71 (14) = happyShift action_6
action_71 (16) = happyShift action_7
action_71 (18) = happyShift action_8
action_71 (51) = happyShift action_9
action_71 (52) = happyShift action_10
action_71 (53) = happyShift action_11
action_71 (54) = happyShift action_12
action_71 (61) = happyShift action_13
action_71 (9) = happyGoto action_92
action_71 _ = happyFail

action_72 (57) = happyShift action_60
action_72 _ = happyReduce_16

action_73 (14) = happyShift action_17
action_73 (16) = happyShift action_18
action_73 (18) = happyShift action_19
action_73 (24) = happyShift action_20
action_73 (26) = happyShift action_46
action_73 (27) = happyShift action_47
action_73 (28) = happyShift action_48
action_73 (29) = happyShift action_49
action_73 (30) = happyShift action_50
action_73 (31) = happyShift action_51
action_73 (32) = happyShift action_52
action_73 (33) = happyShift action_53
action_73 (34) = happyShift action_54
action_73 (35) = happyShift action_55
action_73 (36) = happyShift action_56
action_73 (37) = happyShift action_57
action_73 (38) = happyShift action_58
action_73 (39) = happyShift action_21
action_73 (40) = happyShift action_59
action_73 (41) = happyShift action_22
action_73 (42) = happyShift action_23
action_73 (44) = happyShift action_24
action_73 (47) = happyShift action_25
action_73 (48) = happyShift action_26
action_73 (49) = happyShift action_27
action_73 (50) = happyShift action_28
action_73 (58) = happyShift action_29
action_73 (59) = happyShift action_30
action_73 (60) = happyShift action_31
action_73 (61) = happyShift action_32
action_73 (10) = happyGoto action_44
action_73 (11) = happyGoto action_16
action_73 (13) = happyGoto action_45
action_73 _ = happyReduce_24

action_74 _ = happyReduce_32

action_75 _ = happyReduce_40

action_76 _ = happyReduce_42

action_77 (14) = happyShift action_17
action_77 (16) = happyShift action_18
action_77 (18) = happyShift action_19
action_77 (24) = happyShift action_20
action_77 (39) = happyShift action_21
action_77 (41) = happyShift action_22
action_77 (42) = happyShift action_23
action_77 (44) = happyShift action_24
action_77 (47) = happyShift action_25
action_77 (48) = happyShift action_26
action_77 (49) = happyShift action_27
action_77 (50) = happyShift action_28
action_77 (58) = happyShift action_29
action_77 (59) = happyShift action_30
action_77 (60) = happyShift action_31
action_77 (61) = happyShift action_32
action_77 (10) = happyGoto action_91
action_77 (11) = happyGoto action_16
action_77 _ = happyFail

action_78 (14) = happyShift action_17
action_78 (16) = happyShift action_18
action_78 (18) = happyShift action_19
action_78 (24) = happyShift action_20
action_78 (39) = happyShift action_21
action_78 (41) = happyShift action_22
action_78 (42) = happyShift action_23
action_78 (44) = happyShift action_24
action_78 (47) = happyShift action_25
action_78 (48) = happyShift action_26
action_78 (49) = happyShift action_27
action_78 (50) = happyShift action_28
action_78 (58) = happyShift action_29
action_78 (59) = happyShift action_30
action_78 (60) = happyShift action_31
action_78 (61) = happyShift action_32
action_78 (10) = happyGoto action_90
action_78 (11) = happyGoto action_16
action_78 _ = happyFail

action_79 (14) = happyShift action_17
action_79 (16) = happyShift action_18
action_79 (18) = happyShift action_19
action_79 (23) = happyShift action_89
action_79 (24) = happyShift action_20
action_79 (39) = happyShift action_21
action_79 (41) = happyShift action_22
action_79 (42) = happyShift action_23
action_79 (44) = happyShift action_24
action_79 (47) = happyShift action_25
action_79 (48) = happyShift action_26
action_79 (49) = happyShift action_27
action_79 (50) = happyShift action_28
action_79 (58) = happyShift action_29
action_79 (59) = happyShift action_30
action_79 (60) = happyShift action_31
action_79 (61) = happyShift action_32
action_79 (10) = happyGoto action_88
action_79 (11) = happyGoto action_16
action_79 _ = happyFail

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
action_80 (58) = happyShift action_29
action_80 (59) = happyShift action_30
action_80 (60) = happyShift action_31
action_80 (61) = happyShift action_32
action_80 (10) = happyGoto action_87
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
action_81 (58) = happyShift action_29
action_81 (59) = happyShift action_30
action_81 (60) = happyShift action_31
action_81 (61) = happyShift action_32
action_81 (10) = happyGoto action_86
action_81 (11) = happyGoto action_16
action_81 _ = happyFail

action_82 (14) = happyShift action_17
action_82 (16) = happyShift action_18
action_82 (18) = happyShift action_19
action_82 (24) = happyShift action_20
action_82 (39) = happyShift action_21
action_82 (41) = happyShift action_22
action_82 (42) = happyShift action_23
action_82 (44) = happyShift action_24
action_82 (47) = happyShift action_25
action_82 (48) = happyShift action_26
action_82 (49) = happyShift action_27
action_82 (50) = happyShift action_28
action_82 (58) = happyShift action_29
action_82 (59) = happyShift action_30
action_82 (60) = happyShift action_31
action_82 (61) = happyShift action_32
action_82 (10) = happyGoto action_85
action_82 (11) = happyGoto action_16
action_82 _ = happyFail

action_83 (20) = happyShift action_84
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
action_84 (58) = happyShift action_29
action_84 (59) = happyShift action_30
action_84 (60) = happyShift action_31
action_84 (61) = happyShift action_32
action_84 (10) = happyGoto action_105
action_84 (11) = happyGoto action_16
action_84 _ = happyFail

action_85 (14) = happyShift action_17
action_85 (16) = happyShift action_18
action_85 (18) = happyShift action_19
action_85 (24) = happyShift action_20
action_85 (26) = happyShift action_46
action_85 (27) = happyShift action_47
action_85 (28) = happyShift action_48
action_85 (29) = happyShift action_49
action_85 (30) = happyShift action_50
action_85 (31) = happyShift action_51
action_85 (32) = happyShift action_52
action_85 (33) = happyShift action_53
action_85 (34) = happyShift action_54
action_85 (35) = happyShift action_55
action_85 (36) = happyShift action_56
action_85 (37) = happyShift action_57
action_85 (38) = happyShift action_58
action_85 (39) = happyShift action_21
action_85 (40) = happyShift action_59
action_85 (41) = happyShift action_22
action_85 (42) = happyShift action_23
action_85 (44) = happyShift action_24
action_85 (46) = happyShift action_104
action_85 (47) = happyShift action_25
action_85 (48) = happyShift action_26
action_85 (49) = happyShift action_27
action_85 (50) = happyShift action_28
action_85 (58) = happyShift action_29
action_85 (59) = happyShift action_30
action_85 (60) = happyShift action_31
action_85 (61) = happyShift action_32
action_85 (10) = happyGoto action_44
action_85 (11) = happyGoto action_16
action_85 (13) = happyGoto action_45
action_85 _ = happyFail

action_86 (14) = happyShift action_17
action_86 (16) = happyShift action_18
action_86 (18) = happyShift action_19
action_86 (24) = happyShift action_20
action_86 (26) = happyShift action_46
action_86 (27) = happyShift action_47
action_86 (28) = happyShift action_48
action_86 (29) = happyShift action_49
action_86 (30) = happyShift action_50
action_86 (31) = happyShift action_51
action_86 (32) = happyShift action_52
action_86 (33) = happyShift action_53
action_86 (34) = happyShift action_54
action_86 (35) = happyShift action_55
action_86 (36) = happyShift action_56
action_86 (37) = happyShift action_57
action_86 (38) = happyShift action_58
action_86 (39) = happyShift action_21
action_86 (40) = happyShift action_59
action_86 (41) = happyShift action_22
action_86 (42) = happyShift action_23
action_86 (43) = happyShift action_103
action_86 (44) = happyShift action_24
action_86 (47) = happyShift action_25
action_86 (48) = happyShift action_26
action_86 (49) = happyShift action_27
action_86 (50) = happyShift action_28
action_86 (58) = happyShift action_29
action_86 (59) = happyShift action_30
action_86 (60) = happyShift action_31
action_86 (61) = happyShift action_32
action_86 (10) = happyGoto action_44
action_86 (11) = happyGoto action_16
action_86 (13) = happyGoto action_45
action_86 _ = happyFail

action_87 (24) = happyShift action_20
action_87 (26) = happyShift action_46
action_87 (27) = happyShift action_47
action_87 (28) = happyShift action_48
action_87 (29) = happyShift action_49
action_87 (30) = happyShift action_50
action_87 (31) = happyShift action_51
action_87 (32) = happyShift action_52
action_87 (33) = happyShift action_53
action_87 (34) = happyShift action_54
action_87 (35) = happyShift action_55
action_87 (36) = happyShift action_56
action_87 (37) = happyShift action_57
action_87 (38) = happyShift action_58
action_87 (39) = happyShift action_21
action_87 (40) = happyShift action_59
action_87 (41) = happyShift action_22
action_87 (42) = happyShift action_23
action_87 (44) = happyShift action_24
action_87 (47) = happyShift action_25
action_87 (48) = happyShift action_26
action_87 (49) = happyShift action_27
action_87 (50) = happyShift action_28
action_87 (58) = happyShift action_29
action_87 (59) = happyShift action_30
action_87 (60) = happyShift action_31
action_87 (61) = happyShift action_32
action_87 (10) = happyGoto action_44
action_87 (11) = happyGoto action_16
action_87 (13) = happyGoto action_45
action_87 _ = happyReduce_20

action_88 (14) = happyShift action_17
action_88 (16) = happyShift action_18
action_88 (18) = happyShift action_19
action_88 (19) = happyShift action_102
action_88 (24) = happyShift action_20
action_88 (26) = happyShift action_46
action_88 (27) = happyShift action_47
action_88 (28) = happyShift action_48
action_88 (29) = happyShift action_49
action_88 (30) = happyShift action_50
action_88 (31) = happyShift action_51
action_88 (32) = happyShift action_52
action_88 (33) = happyShift action_53
action_88 (34) = happyShift action_54
action_88 (35) = happyShift action_55
action_88 (36) = happyShift action_56
action_88 (37) = happyShift action_57
action_88 (38) = happyShift action_58
action_88 (39) = happyShift action_21
action_88 (40) = happyShift action_59
action_88 (41) = happyShift action_22
action_88 (42) = happyShift action_23
action_88 (44) = happyShift action_24
action_88 (47) = happyShift action_25
action_88 (48) = happyShift action_26
action_88 (49) = happyShift action_27
action_88 (50) = happyShift action_28
action_88 (58) = happyShift action_29
action_88 (59) = happyShift action_30
action_88 (60) = happyShift action_31
action_88 (61) = happyShift action_32
action_88 (10) = happyGoto action_44
action_88 (11) = happyGoto action_16
action_88 (13) = happyGoto action_45
action_88 _ = happyFail

action_89 (19) = happyShift action_101
action_89 _ = happyFail

action_90 (14) = happyShift action_17
action_90 (16) = happyShift action_18
action_90 (18) = happyShift action_19
action_90 (19) = happyShift action_100
action_90 (24) = happyShift action_20
action_90 (26) = happyShift action_46
action_90 (27) = happyShift action_47
action_90 (28) = happyShift action_48
action_90 (29) = happyShift action_49
action_90 (30) = happyShift action_50
action_90 (31) = happyShift action_51
action_90 (32) = happyShift action_52
action_90 (33) = happyShift action_53
action_90 (34) = happyShift action_54
action_90 (35) = happyShift action_55
action_90 (36) = happyShift action_56
action_90 (37) = happyShift action_57
action_90 (38) = happyShift action_58
action_90 (39) = happyShift action_21
action_90 (40) = happyShift action_59
action_90 (41) = happyShift action_22
action_90 (42) = happyShift action_23
action_90 (44) = happyShift action_24
action_90 (47) = happyShift action_25
action_90 (48) = happyShift action_26
action_90 (49) = happyShift action_27
action_90 (50) = happyShift action_28
action_90 (58) = happyShift action_29
action_90 (59) = happyShift action_30
action_90 (60) = happyShift action_31
action_90 (61) = happyShift action_32
action_90 (10) = happyGoto action_44
action_90 (11) = happyGoto action_16
action_90 (13) = happyGoto action_45
action_90 _ = happyFail

action_91 (14) = happyShift action_17
action_91 (16) = happyShift action_18
action_91 (17) = happyShift action_76
action_91 (18) = happyShift action_19
action_91 (21) = happyShift action_77
action_91 (24) = happyShift action_20
action_91 (26) = happyShift action_46
action_91 (27) = happyShift action_47
action_91 (28) = happyShift action_48
action_91 (29) = happyShift action_49
action_91 (30) = happyShift action_50
action_91 (31) = happyShift action_51
action_91 (32) = happyShift action_52
action_91 (33) = happyShift action_53
action_91 (34) = happyShift action_54
action_91 (35) = happyShift action_55
action_91 (36) = happyShift action_56
action_91 (37) = happyShift action_57
action_91 (38) = happyShift action_58
action_91 (39) = happyShift action_21
action_91 (40) = happyShift action_59
action_91 (41) = happyShift action_22
action_91 (42) = happyShift action_23
action_91 (44) = happyShift action_24
action_91 (47) = happyShift action_25
action_91 (48) = happyShift action_26
action_91 (49) = happyShift action_27
action_91 (50) = happyShift action_28
action_91 (58) = happyShift action_29
action_91 (59) = happyShift action_30
action_91 (60) = happyShift action_31
action_91 (61) = happyShift action_32
action_91 (10) = happyGoto action_44
action_91 (11) = happyGoto action_16
action_91 (12) = happyGoto action_99
action_91 (13) = happyGoto action_45
action_91 _ = happyFail

action_92 (19) = happyShift action_98
action_92 (57) = happyShift action_60
action_92 _ = happyFail

action_93 (19) = happyShift action_97
action_93 (57) = happyShift action_60
action_93 _ = happyFail

action_94 _ = happyReduce_7

action_95 (14) = happyShift action_17
action_95 (16) = happyShift action_18
action_95 (18) = happyShift action_19
action_95 (24) = happyShift action_20
action_95 (39) = happyShift action_21
action_95 (41) = happyShift action_22
action_95 (42) = happyShift action_23
action_95 (44) = happyShift action_24
action_95 (47) = happyShift action_25
action_95 (48) = happyShift action_26
action_95 (49) = happyShift action_27
action_95 (50) = happyShift action_28
action_95 (58) = happyShift action_29
action_95 (59) = happyShift action_30
action_95 (60) = happyShift action_31
action_95 (61) = happyShift action_32
action_95 (10) = happyGoto action_96
action_95 (11) = happyGoto action_16
action_95 _ = happyFail

action_96 (14) = happyShift action_107
action_96 (16) = happyShift action_108
action_96 (18) = happyShift action_109
action_96 (24) = happyShift action_20
action_96 (26) = happyShift action_46
action_96 (27) = happyShift action_47
action_96 (28) = happyShift action_48
action_96 (29) = happyShift action_49
action_96 (30) = happyShift action_50
action_96 (31) = happyShift action_51
action_96 (32) = happyShift action_52
action_96 (33) = happyShift action_53
action_96 (34) = happyShift action_54
action_96 (35) = happyShift action_55
action_96 (36) = happyShift action_56
action_96 (37) = happyShift action_57
action_96 (38) = happyShift action_58
action_96 (39) = happyShift action_21
action_96 (40) = happyShift action_59
action_96 (41) = happyShift action_22
action_96 (42) = happyShift action_23
action_96 (44) = happyShift action_24
action_96 (47) = happyShift action_25
action_96 (48) = happyShift action_26
action_96 (49) = happyShift action_27
action_96 (50) = happyShift action_28
action_96 (51) = happyShift action_9
action_96 (52) = happyShift action_10
action_96 (53) = happyShift action_11
action_96 (54) = happyShift action_12
action_96 (58) = happyShift action_29
action_96 (59) = happyShift action_30
action_96 (60) = happyShift action_31
action_96 (61) = happyShift action_110
action_96 (6) = happyGoto action_106
action_96 (7) = happyGoto action_34
action_96 (9) = happyGoto action_5
action_96 (10) = happyGoto action_44
action_96 (11) = happyGoto action_16
action_96 (13) = happyGoto action_45
action_96 _ = happyFail

action_97 _ = happyReduce_14

action_98 _ = happyReduce_15

action_99 _ = happyReduce_41

action_100 _ = happyReduce_34

action_101 _ = happyReduce_33

action_102 _ = happyReduce_35

action_103 (14) = happyShift action_17
action_103 (16) = happyShift action_18
action_103 (18) = happyShift action_19
action_103 (24) = happyShift action_20
action_103 (39) = happyShift action_21
action_103 (41) = happyShift action_22
action_103 (42) = happyShift action_23
action_103 (44) = happyShift action_24
action_103 (47) = happyShift action_25
action_103 (48) = happyShift action_26
action_103 (49) = happyShift action_27
action_103 (50) = happyShift action_28
action_103 (58) = happyShift action_29
action_103 (59) = happyShift action_30
action_103 (60) = happyShift action_31
action_103 (61) = happyShift action_32
action_103 (10) = happyGoto action_112
action_103 (11) = happyGoto action_16
action_103 _ = happyFail

action_104 (14) = happyShift action_17
action_104 (16) = happyShift action_18
action_104 (18) = happyShift action_19
action_104 (24) = happyShift action_20
action_104 (39) = happyShift action_21
action_104 (41) = happyShift action_22
action_104 (42) = happyShift action_23
action_104 (44) = happyShift action_24
action_104 (47) = happyShift action_25
action_104 (48) = happyShift action_26
action_104 (49) = happyShift action_27
action_104 (50) = happyShift action_28
action_104 (58) = happyShift action_29
action_104 (59) = happyShift action_30
action_104 (60) = happyShift action_31
action_104 (61) = happyShift action_32
action_104 (10) = happyGoto action_111
action_104 (11) = happyGoto action_16
action_104 _ = happyFail

action_105 (14) = happyShift action_107
action_105 (16) = happyShift action_108
action_105 (18) = happyShift action_109
action_105 (24) = happyShift action_20
action_105 (26) = happyShift action_46
action_105 (27) = happyShift action_47
action_105 (28) = happyShift action_48
action_105 (29) = happyShift action_49
action_105 (30) = happyShift action_50
action_105 (31) = happyShift action_51
action_105 (32) = happyShift action_52
action_105 (33) = happyShift action_53
action_105 (34) = happyShift action_54
action_105 (35) = happyShift action_55
action_105 (36) = happyShift action_56
action_105 (37) = happyShift action_57
action_105 (38) = happyShift action_58
action_105 (39) = happyShift action_21
action_105 (40) = happyShift action_59
action_105 (41) = happyShift action_22
action_105 (42) = happyShift action_23
action_105 (44) = happyShift action_24
action_105 (47) = happyShift action_25
action_105 (48) = happyShift action_26
action_105 (49) = happyShift action_27
action_105 (50) = happyShift action_28
action_105 (51) = happyShift action_9
action_105 (52) = happyShift action_10
action_105 (53) = happyShift action_11
action_105 (54) = happyShift action_12
action_105 (58) = happyShift action_29
action_105 (59) = happyShift action_30
action_105 (60) = happyShift action_31
action_105 (61) = happyShift action_110
action_105 (6) = happyGoto action_106
action_105 (7) = happyGoto action_34
action_105 (9) = happyGoto action_5
action_105 (10) = happyGoto action_44
action_105 (11) = happyGoto action_16
action_105 (13) = happyGoto action_45
action_105 _ = happyReduce_4

action_106 _ = happyReduce_3

action_107 (14) = happyShift action_107
action_107 (16) = happyShift action_108
action_107 (18) = happyShift action_109
action_107 (24) = happyShift action_20
action_107 (39) = happyShift action_21
action_107 (41) = happyShift action_22
action_107 (42) = happyShift action_23
action_107 (44) = happyShift action_24
action_107 (47) = happyShift action_25
action_107 (48) = happyShift action_26
action_107 (49) = happyShift action_27
action_107 (50) = happyShift action_28
action_107 (51) = happyShift action_9
action_107 (52) = happyShift action_10
action_107 (53) = happyShift action_11
action_107 (54) = happyShift action_12
action_107 (58) = happyShift action_29
action_107 (59) = happyShift action_30
action_107 (60) = happyShift action_31
action_107 (61) = happyShift action_110
action_107 (9) = happyGoto action_63
action_107 (10) = happyGoto action_43
action_107 (11) = happyGoto action_16
action_107 _ = happyFail

action_108 (14) = happyShift action_107
action_108 (16) = happyShift action_108
action_108 (17) = happyShift action_42
action_108 (18) = happyShift action_109
action_108 (24) = happyShift action_20
action_108 (39) = happyShift action_21
action_108 (41) = happyShift action_22
action_108 (42) = happyShift action_23
action_108 (44) = happyShift action_24
action_108 (47) = happyShift action_25
action_108 (48) = happyShift action_26
action_108 (49) = happyShift action_27
action_108 (50) = happyShift action_28
action_108 (51) = happyShift action_9
action_108 (52) = happyShift action_10
action_108 (53) = happyShift action_11
action_108 (54) = happyShift action_12
action_108 (58) = happyShift action_29
action_108 (59) = happyShift action_30
action_108 (60) = happyShift action_31
action_108 (61) = happyShift action_110
action_108 (9) = happyGoto action_62
action_108 (10) = happyGoto action_41
action_108 (11) = happyGoto action_16
action_108 _ = happyFail

action_109 (14) = happyShift action_107
action_109 (16) = happyShift action_108
action_109 (18) = happyShift action_109
action_109 (23) = happyShift action_40
action_109 (24) = happyShift action_20
action_109 (39) = happyShift action_21
action_109 (41) = happyShift action_22
action_109 (42) = happyShift action_23
action_109 (44) = happyShift action_24
action_109 (47) = happyShift action_25
action_109 (48) = happyShift action_26
action_109 (49) = happyShift action_27
action_109 (50) = happyShift action_28
action_109 (51) = happyShift action_9
action_109 (52) = happyShift action_10
action_109 (53) = happyShift action_11
action_109 (54) = happyShift action_12
action_109 (58) = happyShift action_29
action_109 (59) = happyShift action_30
action_109 (60) = happyShift action_31
action_109 (61) = happyShift action_110
action_109 (9) = happyGoto action_61
action_109 (10) = happyGoto action_39
action_109 (11) = happyGoto action_16
action_109 _ = happyFail

action_110 (15) = happyReduce_18
action_110 (17) = happyReduce_18
action_110 (22) = happyReduce_18
action_110 (55) = happyReduce_18
action_110 (56) = happyReduce_18
action_110 (57) = happyReduce_18
action_110 _ = happyReduce_22

action_111 (14) = happyShift action_17
action_111 (16) = happyShift action_18
action_111 (18) = happyShift action_19
action_111 (24) = happyShift action_20
action_111 (26) = happyShift action_46
action_111 (27) = happyShift action_47
action_111 (28) = happyShift action_48
action_111 (29) = happyShift action_49
action_111 (30) = happyShift action_50
action_111 (31) = happyShift action_51
action_111 (32) = happyShift action_52
action_111 (33) = happyShift action_53
action_111 (34) = happyShift action_54
action_111 (35) = happyShift action_55
action_111 (36) = happyShift action_56
action_111 (37) = happyShift action_57
action_111 (38) = happyShift action_58
action_111 (39) = happyShift action_21
action_111 (40) = happyShift action_59
action_111 (41) = happyShift action_22
action_111 (42) = happyShift action_23
action_111 (44) = happyShift action_24
action_111 (47) = happyShift action_25
action_111 (48) = happyShift action_26
action_111 (49) = happyShift action_27
action_111 (50) = happyShift action_28
action_111 (58) = happyShift action_29
action_111 (59) = happyShift action_30
action_111 (60) = happyShift action_31
action_111 (61) = happyShift action_32
action_111 (10) = happyGoto action_44
action_111 (11) = happyGoto action_16
action_111 (13) = happyGoto action_45
action_111 _ = happyReduce_23

action_112 (24) = happyShift action_20
action_112 (26) = happyShift action_46
action_112 (27) = happyShift action_47
action_112 (28) = happyShift action_48
action_112 (29) = happyShift action_49
action_112 (30) = happyShift action_50
action_112 (31) = happyShift action_51
action_112 (32) = happyShift action_52
action_112 (33) = happyShift action_53
action_112 (34) = happyShift action_54
action_112 (35) = happyShift action_55
action_112 (36) = happyShift action_56
action_112 (37) = happyShift action_57
action_112 (38) = happyShift action_58
action_112 (39) = happyShift action_21
action_112 (40) = happyShift action_59
action_112 (41) = happyShift action_22
action_112 (42) = happyShift action_23
action_112 (44) = happyShift action_24
action_112 (47) = happyShift action_25
action_112 (48) = happyShift action_26
action_112 (49) = happyShift action_27
action_112 (50) = happyShift action_28
action_112 (58) = happyShift action_29
action_112 (59) = happyShift action_30
action_112 (60) = happyShift action_31
action_112 (61) = happyShift action_32
action_112 (10) = happyGoto action_44
action_112 (11) = happyGoto action_16
action_112 (13) = happyGoto action_45
action_112 _ = happyReduce_19

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
happyReduction_31 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  10 happyReduction_32
happyReduction_32 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happyReduce 5 10 happyReduction_33
happyReduction_33 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (App (Operation InjL) happy_var_2
	) `HappyStk` happyRest

happyReduce_34 = happyReduce 5 10 happyReduction_34
happyReduction_34 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (App (Operation InjR) happy_var_4
	) `HappyStk` happyRest

happyReduce_35 = happyReduce 5 10 happyReduction_35
happyReduction_35 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (App (App (Operation Tuple) happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_36 = happySpecReduce_1  10 happyReduction_36
happyReduction_36 (HappyTerminal (TokenInt    p happy_var_1))
	 =  HappyAbsSyn10
		 (Constant (IntVal happy_var_1)
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  10 happyReduction_37
happyReduction_37 (HappyTerminal (TokenBool   p happy_var_1))
	 =  HappyAbsSyn10
		 (Constant (BoolVal happy_var_1)
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  10 happyReduction_38
happyReduction_38 (HappyTerminal (TokenStr    p happy_var_1))
	 =  HappyAbsSyn10
		 (parseStr happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_2  11 happyReduction_39
happyReduction_39 _
	_
	 =  HappyAbsSyn10
		 (Operation Empty
	)

happyReduce_40 = happySpecReduce_3  11 happyReduction_40
happyReduction_40 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (App (App (Operation Cons) happy_var_2) happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  12 happyReduction_41
happyReduction_41 (HappyAbsSyn10  happy_var_3)
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (App (App (Operation Cons) happy_var_2) happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  12 happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn10
		 (Operation Empty
	)

happyReduce_43 = happySpecReduce_1  13 happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn10
		 (Operation Add
	)

happyReduce_44 = happySpecReduce_1  13 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn10
		 (Operation Sub
	)

happyReduce_45 = happySpecReduce_1  13 happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn10
		 (Operation Mul
	)

happyReduce_46 = happySpecReduce_1  13 happyReduction_46
happyReduction_46 _
	 =  HappyAbsSyn10
		 (Operation Div
	)

happyReduce_47 = happySpecReduce_1  13 happyReduction_47
happyReduction_47 _
	 =  HappyAbsSyn10
		 (Operation Mod
	)

happyReduce_48 = happySpecReduce_1  13 happyReduction_48
happyReduction_48 _
	 =  HappyAbsSyn10
		 (Operation And
	)

happyReduce_49 = happySpecReduce_1  13 happyReduction_49
happyReduction_49 _
	 =  HappyAbsSyn10
		 (Operation Or
	)

happyReduce_50 = happySpecReduce_1  13 happyReduction_50
happyReduction_50 _
	 =  HappyAbsSyn10
		 (Operation Xor
	)

happyReduce_51 = happySpecReduce_1  13 happyReduction_51
happyReduction_51 _
	 =  HappyAbsSyn10
		 (Operation Lss
	)

happyReduce_52 = happySpecReduce_1  13 happyReduction_52
happyReduction_52 _
	 =  HappyAbsSyn10
		 (Operation LsE
	)

happyReduce_53 = happySpecReduce_1  13 happyReduction_53
happyReduction_53 _
	 =  HappyAbsSyn10
		 (Operation Equ
	)

happyReduce_54 = happySpecReduce_1  13 happyReduction_54
happyReduction_54 _
	 =  HappyAbsSyn10
		 (Operation NEq
	)

happyReduce_55 = happySpecReduce_1  13 happyReduction_55
happyReduction_55 _
	 =  HappyAbsSyn10
		 (Operation Gtr
	)

happyReduce_56 = happySpecReduce_1  13 happyReduction_56
happyReduction_56 _
	 =  HappyAbsSyn10
		 (Operation GtE
	)

happyNewToken action sts stk [] =
	action 63 63 notHappyAtAll (HappyState action) sts stk []

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
	TokenTyInt  p -> cont 51;
	TokenTyBool p -> cont 52;
	TokenTyChar p -> cont 53;
	TokenTyStr  p -> cont 54;
	TokenTySum  p -> cont 55;
	TokenTyProd p -> cont 56;
	TokenTyArrw p -> cont 57;
	TokenInt    p happy_dollar_dollar -> cont 58;
	TokenBool   p happy_dollar_dollar -> cont 59;
	TokenStr    p happy_dollar_dollar -> cont 60;
	TokenIdLC   p happy_dollar_dollar -> cont 61;
	TokenIdUC   p happy_dollar_dollar -> cont 62;
	_ -> happyError' (tk:tks)
	}

happyError_ 63 tk tks = happyError' tks
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
