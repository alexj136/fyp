{-# OPTIONS_GHC -w #-}
module Parser where

import Lexer
import Syntax

{-- CONTEXT FREE GRAMMAR

PROG ::= PROG TYDC
       | PROG ID ARGS = EXP
       | epsilon
TYDC ::= ID : TY
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

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 ([Decl])
	| HappyAbsSyn5 (Maybe Type)
	| HappyAbsSyn6 ([String])
	| HappyAbsSyn7 (Type)
	| HappyAbsSyn8 (TypedExp)

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
 action_99 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
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
 happyReduce_48 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (55) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_5
action_0 _ = happyFail

action_1 (55) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail

action_2 (55) = happyShift action_8
action_2 _ = happyFail

action_3 (20) = happyShift action_7
action_3 _ = happyFail

action_4 (57) = happyAccept
action_4 _ = happyFail

action_5 (55) = happyShift action_6
action_5 _ = happyFail

action_6 (55) = happyShift action_10
action_6 (6) = happyGoto action_20
action_6 _ = happyReduce_6

action_7 (12) = happyShift action_12
action_7 (14) = happyShift action_13
action_7 (16) = happyShift action_14
action_7 (45) = happyShift action_15
action_7 (46) = happyShift action_16
action_7 (47) = happyShift action_17
action_7 (48) = happyShift action_18
action_7 (55) = happyShift action_19
action_7 (7) = happyGoto action_11
action_7 _ = happyFail

action_8 (55) = happyShift action_10
action_8 (6) = happyGoto action_9
action_8 _ = happyFail

action_9 (18) = happyShift action_27
action_9 _ = happyFail

action_10 (55) = happyShift action_10
action_10 (6) = happyGoto action_26
action_10 _ = happyReduce_6

action_11 (51) = happyShift action_25
action_11 _ = happyReduce_3

action_12 (12) = happyShift action_12
action_12 (14) = happyShift action_13
action_12 (16) = happyShift action_14
action_12 (45) = happyShift action_15
action_12 (46) = happyShift action_16
action_12 (47) = happyShift action_17
action_12 (48) = happyShift action_18
action_12 (55) = happyShift action_19
action_12 (7) = happyGoto action_24
action_12 _ = happyFail

action_13 (12) = happyShift action_12
action_13 (14) = happyShift action_13
action_13 (16) = happyShift action_14
action_13 (45) = happyShift action_15
action_13 (46) = happyShift action_16
action_13 (47) = happyShift action_17
action_13 (48) = happyShift action_18
action_13 (55) = happyShift action_19
action_13 (7) = happyGoto action_23
action_13 _ = happyFail

action_14 (12) = happyShift action_12
action_14 (14) = happyShift action_13
action_14 (16) = happyShift action_14
action_14 (45) = happyShift action_15
action_14 (46) = happyShift action_16
action_14 (47) = happyShift action_17
action_14 (48) = happyShift action_18
action_14 (55) = happyShift action_19
action_14 (7) = happyGoto action_22
action_14 _ = happyFail

action_15 _ = happyReduce_7

action_16 _ = happyReduce_8

action_17 _ = happyReduce_9

action_18 _ = happyReduce_10

action_19 _ = happyReduce_16

action_20 (18) = happyShift action_21
action_20 _ = happyFail

action_21 (12) = happyShift action_30
action_21 (14) = happyShift action_31
action_21 (16) = happyShift action_32
action_21 (22) = happyShift action_33
action_21 (37) = happyShift action_34
action_21 (39) = happyShift action_35
action_21 (40) = happyShift action_36
action_21 (42) = happyShift action_37
action_21 (52) = happyShift action_38
action_21 (53) = happyShift action_39
action_21 (55) = happyShift action_40
action_21 (8) = happyGoto action_46
action_21 (9) = happyGoto action_29
action_21 _ = happyFail

action_22 (49) = happyShift action_44
action_22 (50) = happyShift action_45
action_22 (51) = happyShift action_25
action_22 _ = happyFail

action_23 (15) = happyShift action_43
action_23 (51) = happyShift action_25
action_23 _ = happyFail

action_24 (13) = happyShift action_42
action_24 (51) = happyShift action_25
action_24 _ = happyFail

action_25 (12) = happyShift action_12
action_25 (14) = happyShift action_13
action_25 (16) = happyShift action_14
action_25 (45) = happyShift action_15
action_25 (46) = happyShift action_16
action_25 (47) = happyShift action_17
action_25 (48) = happyShift action_18
action_25 (55) = happyShift action_19
action_25 (7) = happyGoto action_41
action_25 _ = happyFail

action_26 _ = happyReduce_5

action_27 (12) = happyShift action_30
action_27 (14) = happyShift action_31
action_27 (16) = happyShift action_32
action_27 (22) = happyShift action_33
action_27 (37) = happyShift action_34
action_27 (39) = happyShift action_35
action_27 (40) = happyShift action_36
action_27 (42) = happyShift action_37
action_27 (52) = happyShift action_38
action_27 (53) = happyShift action_39
action_27 (55) = happyShift action_40
action_27 (8) = happyGoto action_28
action_27 (9) = happyGoto action_29
action_27 _ = happyFail

action_28 (12) = happyShift action_30
action_28 (14) = happyShift action_31
action_28 (16) = happyShift action_32
action_28 (22) = happyShift action_33
action_28 (24) = happyShift action_50
action_28 (25) = happyShift action_51
action_28 (26) = happyShift action_52
action_28 (27) = happyShift action_53
action_28 (28) = happyShift action_54
action_28 (29) = happyShift action_55
action_28 (30) = happyShift action_56
action_28 (31) = happyShift action_57
action_28 (32) = happyShift action_58
action_28 (33) = happyShift action_59
action_28 (34) = happyShift action_60
action_28 (35) = happyShift action_61
action_28 (36) = happyShift action_62
action_28 (37) = happyShift action_34
action_28 (38) = happyShift action_63
action_28 (39) = happyShift action_35
action_28 (40) = happyShift action_36
action_28 (42) = happyShift action_37
action_28 (52) = happyShift action_38
action_28 (53) = happyShift action_39
action_28 (55) = happyShift action_64
action_28 (4) = happyGoto action_47
action_28 (5) = happyGoto action_5
action_28 (8) = happyGoto action_48
action_28 (9) = happyGoto action_29
action_28 (11) = happyGoto action_49
action_28 _ = happyFail

action_29 _ = happyReduce_25

action_30 (12) = happyShift action_30
action_30 (14) = happyShift action_31
action_30 (16) = happyShift action_32
action_30 (22) = happyShift action_33
action_30 (37) = happyShift action_34
action_30 (39) = happyShift action_35
action_30 (40) = happyShift action_36
action_30 (42) = happyShift action_37
action_30 (52) = happyShift action_38
action_30 (53) = happyShift action_39
action_30 (55) = happyShift action_40
action_30 (8) = happyGoto action_74
action_30 (9) = happyGoto action_29
action_30 _ = happyFail

action_31 (12) = happyShift action_30
action_31 (14) = happyShift action_31
action_31 (15) = happyShift action_73
action_31 (16) = happyShift action_32
action_31 (22) = happyShift action_33
action_31 (37) = happyShift action_34
action_31 (39) = happyShift action_35
action_31 (40) = happyShift action_36
action_31 (42) = happyShift action_37
action_31 (52) = happyShift action_38
action_31 (53) = happyShift action_39
action_31 (55) = happyShift action_40
action_31 (8) = happyGoto action_72
action_31 (9) = happyGoto action_29
action_31 _ = happyFail

action_32 (12) = happyShift action_30
action_32 (14) = happyShift action_31
action_32 (16) = happyShift action_32
action_32 (21) = happyShift action_71
action_32 (22) = happyShift action_33
action_32 (37) = happyShift action_34
action_32 (39) = happyShift action_35
action_32 (40) = happyShift action_36
action_32 (42) = happyShift action_37
action_32 (52) = happyShift action_38
action_32 (53) = happyShift action_39
action_32 (55) = happyShift action_40
action_32 (8) = happyGoto action_70
action_32 (9) = happyGoto action_29
action_32 _ = happyFail

action_33 (55) = happyShift action_69
action_33 _ = happyFail

action_34 _ = happyReduce_24

action_35 _ = happyReduce_23

action_36 (55) = happyShift action_68
action_36 _ = happyFail

action_37 (12) = happyShift action_30
action_37 (14) = happyShift action_31
action_37 (16) = happyShift action_32
action_37 (22) = happyShift action_33
action_37 (37) = happyShift action_34
action_37 (39) = happyShift action_35
action_37 (40) = happyShift action_36
action_37 (42) = happyShift action_37
action_37 (52) = happyShift action_38
action_37 (53) = happyShift action_39
action_37 (55) = happyShift action_40
action_37 (8) = happyGoto action_67
action_37 (9) = happyGoto action_29
action_37 _ = happyFail

action_38 _ = happyReduce_29

action_39 _ = happyReduce_30

action_40 _ = happyReduce_20

action_41 (51) = happyShift action_25
action_41 _ = happyReduce_14

action_42 _ = happyReduce_15

action_43 _ = happyReduce_11

action_44 (12) = happyShift action_12
action_44 (14) = happyShift action_13
action_44 (16) = happyShift action_14
action_44 (45) = happyShift action_15
action_44 (46) = happyShift action_16
action_44 (47) = happyShift action_17
action_44 (48) = happyShift action_18
action_44 (55) = happyShift action_19
action_44 (7) = happyGoto action_66
action_44 _ = happyFail

action_45 (12) = happyShift action_12
action_45 (14) = happyShift action_13
action_45 (16) = happyShift action_14
action_45 (45) = happyShift action_15
action_45 (46) = happyShift action_16
action_45 (47) = happyShift action_17
action_45 (48) = happyShift action_18
action_45 (55) = happyShift action_19
action_45 (7) = happyGoto action_65
action_45 _ = happyFail

action_46 (12) = happyShift action_30
action_46 (14) = happyShift action_31
action_46 (16) = happyShift action_32
action_46 (22) = happyShift action_33
action_46 (24) = happyShift action_50
action_46 (25) = happyShift action_51
action_46 (26) = happyShift action_52
action_46 (27) = happyShift action_53
action_46 (28) = happyShift action_54
action_46 (29) = happyShift action_55
action_46 (30) = happyShift action_56
action_46 (31) = happyShift action_57
action_46 (32) = happyShift action_58
action_46 (33) = happyShift action_59
action_46 (34) = happyShift action_60
action_46 (35) = happyShift action_61
action_46 (36) = happyShift action_62
action_46 (37) = happyShift action_34
action_46 (38) = happyShift action_63
action_46 (39) = happyShift action_35
action_46 (40) = happyShift action_36
action_46 (42) = happyShift action_37
action_46 (52) = happyShift action_38
action_46 (53) = happyShift action_39
action_46 (55) = happyShift action_64
action_46 (4) = happyGoto action_47
action_46 (5) = happyGoto action_5
action_46 (8) = happyGoto action_48
action_46 (9) = happyGoto action_29
action_46 (11) = happyGoto action_49
action_46 _ = happyReduce_2

action_47 _ = happyReduce_1

action_48 (12) = happyShift action_30
action_48 (14) = happyShift action_31
action_48 (16) = happyShift action_32
action_48 (22) = happyShift action_33
action_48 (24) = happyShift action_50
action_48 (25) = happyShift action_51
action_48 (26) = happyShift action_52
action_48 (27) = happyShift action_53
action_48 (28) = happyShift action_54
action_48 (29) = happyShift action_55
action_48 (30) = happyShift action_56
action_48 (31) = happyShift action_57
action_48 (32) = happyShift action_58
action_48 (33) = happyShift action_59
action_48 (34) = happyShift action_60
action_48 (35) = happyShift action_61
action_48 (36) = happyShift action_62
action_48 (37) = happyShift action_34
action_48 (38) = happyShift action_63
action_48 (39) = happyShift action_35
action_48 (40) = happyShift action_36
action_48 (42) = happyShift action_37
action_48 (52) = happyShift action_38
action_48 (53) = happyShift action_39
action_48 (55) = happyShift action_40
action_48 (8) = happyGoto action_48
action_48 (9) = happyGoto action_29
action_48 (11) = happyGoto action_49
action_48 _ = happyReduce_19

action_49 (12) = happyShift action_30
action_49 (14) = happyShift action_31
action_49 (16) = happyShift action_32
action_49 (22) = happyShift action_33
action_49 (37) = happyShift action_34
action_49 (39) = happyShift action_35
action_49 (40) = happyShift action_36
action_49 (42) = happyShift action_37
action_49 (52) = happyShift action_38
action_49 (53) = happyShift action_39
action_49 (55) = happyShift action_40
action_49 (8) = happyGoto action_86
action_49 (9) = happyGoto action_29
action_49 _ = happyFail

action_50 _ = happyReduce_35

action_51 _ = happyReduce_36

action_52 _ = happyReduce_37

action_53 _ = happyReduce_38

action_54 _ = happyReduce_39

action_55 _ = happyReduce_45

action_56 _ = happyReduce_44

action_57 _ = happyReduce_43

action_58 _ = happyReduce_48

action_59 _ = happyReduce_47

action_60 _ = happyReduce_46

action_61 _ = happyReduce_40

action_62 _ = happyReduce_41

action_63 _ = happyReduce_42

action_64 (20) = happyShift action_7
action_64 _ = happyReduce_20

action_65 (17) = happyShift action_85
action_65 (51) = happyShift action_25
action_65 _ = happyFail

action_66 (17) = happyShift action_84
action_66 (51) = happyShift action_25
action_66 _ = happyFail

action_67 (12) = happyShift action_30
action_67 (14) = happyShift action_31
action_67 (16) = happyShift action_32
action_67 (22) = happyShift action_33
action_67 (24) = happyShift action_50
action_67 (25) = happyShift action_51
action_67 (26) = happyShift action_52
action_67 (27) = happyShift action_53
action_67 (28) = happyShift action_54
action_67 (29) = happyShift action_55
action_67 (30) = happyShift action_56
action_67 (31) = happyShift action_57
action_67 (32) = happyShift action_58
action_67 (33) = happyShift action_59
action_67 (34) = happyShift action_60
action_67 (35) = happyShift action_61
action_67 (36) = happyShift action_62
action_67 (37) = happyShift action_34
action_67 (38) = happyShift action_63
action_67 (39) = happyShift action_35
action_67 (40) = happyShift action_36
action_67 (42) = happyShift action_37
action_67 (43) = happyShift action_83
action_67 (52) = happyShift action_38
action_67 (53) = happyShift action_39
action_67 (55) = happyShift action_40
action_67 (8) = happyGoto action_48
action_67 (9) = happyGoto action_29
action_67 (11) = happyGoto action_49
action_67 _ = happyFail

action_68 (18) = happyShift action_82
action_68 _ = happyFail

action_69 (23) = happyShift action_81
action_69 _ = happyFail

action_70 (12) = happyShift action_30
action_70 (14) = happyShift action_31
action_70 (16) = happyShift action_32
action_70 (19) = happyShift action_80
action_70 (22) = happyShift action_33
action_70 (24) = happyShift action_50
action_70 (25) = happyShift action_51
action_70 (26) = happyShift action_52
action_70 (27) = happyShift action_53
action_70 (28) = happyShift action_54
action_70 (29) = happyShift action_55
action_70 (30) = happyShift action_56
action_70 (31) = happyShift action_57
action_70 (32) = happyShift action_58
action_70 (33) = happyShift action_59
action_70 (34) = happyShift action_60
action_70 (35) = happyShift action_61
action_70 (36) = happyShift action_62
action_70 (37) = happyShift action_34
action_70 (38) = happyShift action_63
action_70 (39) = happyShift action_35
action_70 (40) = happyShift action_36
action_70 (42) = happyShift action_37
action_70 (52) = happyShift action_38
action_70 (53) = happyShift action_39
action_70 (55) = happyShift action_40
action_70 (8) = happyGoto action_48
action_70 (9) = happyGoto action_29
action_70 (11) = happyGoto action_49
action_70 _ = happyFail

action_71 (19) = happyShift action_79
action_71 _ = happyFail

action_72 (12) = happyShift action_30
action_72 (14) = happyShift action_31
action_72 (15) = happyShift action_77
action_72 (16) = happyShift action_32
action_72 (19) = happyShift action_78
action_72 (22) = happyShift action_33
action_72 (24) = happyShift action_50
action_72 (25) = happyShift action_51
action_72 (26) = happyShift action_52
action_72 (27) = happyShift action_53
action_72 (28) = happyShift action_54
action_72 (29) = happyShift action_55
action_72 (30) = happyShift action_56
action_72 (31) = happyShift action_57
action_72 (32) = happyShift action_58
action_72 (33) = happyShift action_59
action_72 (34) = happyShift action_60
action_72 (35) = happyShift action_61
action_72 (36) = happyShift action_62
action_72 (37) = happyShift action_34
action_72 (38) = happyShift action_63
action_72 (39) = happyShift action_35
action_72 (40) = happyShift action_36
action_72 (42) = happyShift action_37
action_72 (52) = happyShift action_38
action_72 (53) = happyShift action_39
action_72 (55) = happyShift action_40
action_72 (8) = happyGoto action_48
action_72 (9) = happyGoto action_29
action_72 (10) = happyGoto action_76
action_72 (11) = happyGoto action_49
action_72 _ = happyFail

action_73 _ = happyReduce_31

action_74 (12) = happyShift action_30
action_74 (13) = happyShift action_75
action_74 (14) = happyShift action_31
action_74 (16) = happyShift action_32
action_74 (22) = happyShift action_33
action_74 (24) = happyShift action_50
action_74 (25) = happyShift action_51
action_74 (26) = happyShift action_52
action_74 (27) = happyShift action_53
action_74 (28) = happyShift action_54
action_74 (29) = happyShift action_55
action_74 (30) = happyShift action_56
action_74 (31) = happyShift action_57
action_74 (32) = happyShift action_58
action_74 (33) = happyShift action_59
action_74 (34) = happyShift action_60
action_74 (35) = happyShift action_61
action_74 (36) = happyShift action_62
action_74 (37) = happyShift action_34
action_74 (38) = happyShift action_63
action_74 (39) = happyShift action_35
action_74 (40) = happyShift action_36
action_74 (42) = happyShift action_37
action_74 (52) = happyShift action_38
action_74 (53) = happyShift action_39
action_74 (55) = happyShift action_40
action_74 (8) = happyGoto action_48
action_74 (9) = happyGoto action_29
action_74 (11) = happyGoto action_49
action_74 _ = happyFail

action_75 _ = happyReduce_26

action_76 _ = happyReduce_32

action_77 _ = happyReduce_34

action_78 (12) = happyShift action_30
action_78 (14) = happyShift action_31
action_78 (16) = happyShift action_32
action_78 (22) = happyShift action_33
action_78 (37) = happyShift action_34
action_78 (39) = happyShift action_35
action_78 (40) = happyShift action_36
action_78 (42) = happyShift action_37
action_78 (52) = happyShift action_38
action_78 (53) = happyShift action_39
action_78 (55) = happyShift action_40
action_78 (8) = happyGoto action_92
action_78 (9) = happyGoto action_29
action_78 _ = happyFail

action_79 (12) = happyShift action_30
action_79 (14) = happyShift action_31
action_79 (16) = happyShift action_32
action_79 (22) = happyShift action_33
action_79 (37) = happyShift action_34
action_79 (39) = happyShift action_35
action_79 (40) = happyShift action_36
action_79 (42) = happyShift action_37
action_79 (52) = happyShift action_38
action_79 (53) = happyShift action_39
action_79 (55) = happyShift action_40
action_79 (8) = happyGoto action_91
action_79 (9) = happyGoto action_29
action_79 _ = happyFail

action_80 (21) = happyShift action_90
action_80 _ = happyFail

action_81 (12) = happyShift action_30
action_81 (14) = happyShift action_31
action_81 (16) = happyShift action_32
action_81 (22) = happyShift action_33
action_81 (37) = happyShift action_34
action_81 (39) = happyShift action_35
action_81 (40) = happyShift action_36
action_81 (42) = happyShift action_37
action_81 (52) = happyShift action_38
action_81 (53) = happyShift action_39
action_81 (55) = happyShift action_40
action_81 (8) = happyGoto action_89
action_81 (9) = happyGoto action_29
action_81 _ = happyFail

action_82 (12) = happyShift action_30
action_82 (14) = happyShift action_31
action_82 (16) = happyShift action_32
action_82 (22) = happyShift action_33
action_82 (37) = happyShift action_34
action_82 (39) = happyShift action_35
action_82 (40) = happyShift action_36
action_82 (42) = happyShift action_37
action_82 (52) = happyShift action_38
action_82 (53) = happyShift action_39
action_82 (55) = happyShift action_40
action_82 (8) = happyGoto action_88
action_82 (9) = happyGoto action_29
action_82 _ = happyFail

action_83 (12) = happyShift action_30
action_83 (14) = happyShift action_31
action_83 (16) = happyShift action_32
action_83 (22) = happyShift action_33
action_83 (37) = happyShift action_34
action_83 (39) = happyShift action_35
action_83 (40) = happyShift action_36
action_83 (42) = happyShift action_37
action_83 (52) = happyShift action_38
action_83 (53) = happyShift action_39
action_83 (55) = happyShift action_40
action_83 (8) = happyGoto action_87
action_83 (9) = happyGoto action_29
action_83 _ = happyFail

action_84 _ = happyReduce_12

action_85 _ = happyReduce_13

action_86 (12) = happyShift action_30
action_86 (14) = happyShift action_31
action_86 (16) = happyShift action_32
action_86 (22) = happyShift action_33
action_86 (24) = happyShift action_50
action_86 (25) = happyShift action_51
action_86 (26) = happyShift action_52
action_86 (27) = happyShift action_53
action_86 (28) = happyShift action_54
action_86 (29) = happyShift action_55
action_86 (30) = happyShift action_56
action_86 (31) = happyShift action_57
action_86 (32) = happyShift action_58
action_86 (33) = happyShift action_59
action_86 (34) = happyShift action_60
action_86 (35) = happyShift action_61
action_86 (36) = happyShift action_62
action_86 (37) = happyShift action_34
action_86 (38) = happyShift action_63
action_86 (39) = happyShift action_35
action_86 (40) = happyShift action_36
action_86 (42) = happyShift action_37
action_86 (52) = happyShift action_38
action_86 (53) = happyShift action_39
action_86 (55) = happyShift action_40
action_86 (8) = happyGoto action_48
action_86 (9) = happyGoto action_29
action_86 (11) = happyGoto action_49
action_86 _ = happyReduce_22

action_87 (12) = happyShift action_30
action_87 (14) = happyShift action_31
action_87 (16) = happyShift action_32
action_87 (22) = happyShift action_33
action_87 (24) = happyShift action_50
action_87 (25) = happyShift action_51
action_87 (26) = happyShift action_52
action_87 (27) = happyShift action_53
action_87 (28) = happyShift action_54
action_87 (29) = happyShift action_55
action_87 (30) = happyShift action_56
action_87 (31) = happyShift action_57
action_87 (32) = happyShift action_58
action_87 (33) = happyShift action_59
action_87 (34) = happyShift action_60
action_87 (35) = happyShift action_61
action_87 (36) = happyShift action_62
action_87 (37) = happyShift action_34
action_87 (38) = happyShift action_63
action_87 (39) = happyShift action_35
action_87 (40) = happyShift action_36
action_87 (42) = happyShift action_37
action_87 (44) = happyShift action_97
action_87 (52) = happyShift action_38
action_87 (53) = happyShift action_39
action_87 (55) = happyShift action_40
action_87 (8) = happyGoto action_48
action_87 (9) = happyGoto action_29
action_87 (11) = happyGoto action_49
action_87 _ = happyFail

action_88 (12) = happyShift action_30
action_88 (14) = happyShift action_31
action_88 (16) = happyShift action_32
action_88 (22) = happyShift action_33
action_88 (24) = happyShift action_50
action_88 (25) = happyShift action_51
action_88 (26) = happyShift action_52
action_88 (27) = happyShift action_53
action_88 (28) = happyShift action_54
action_88 (29) = happyShift action_55
action_88 (30) = happyShift action_56
action_88 (31) = happyShift action_57
action_88 (32) = happyShift action_58
action_88 (33) = happyShift action_59
action_88 (34) = happyShift action_60
action_88 (35) = happyShift action_61
action_88 (36) = happyShift action_62
action_88 (37) = happyShift action_34
action_88 (38) = happyShift action_63
action_88 (39) = happyShift action_35
action_88 (40) = happyShift action_36
action_88 (41) = happyShift action_96
action_88 (42) = happyShift action_37
action_88 (52) = happyShift action_38
action_88 (53) = happyShift action_39
action_88 (55) = happyShift action_40
action_88 (8) = happyGoto action_48
action_88 (9) = happyGoto action_29
action_88 (11) = happyGoto action_49
action_88 _ = happyFail

action_89 (12) = happyShift action_30
action_89 (14) = happyShift action_31
action_89 (16) = happyShift action_32
action_89 (22) = happyShift action_33
action_89 (24) = happyShift action_50
action_89 (25) = happyShift action_51
action_89 (26) = happyShift action_52
action_89 (27) = happyShift action_53
action_89 (28) = happyShift action_54
action_89 (29) = happyShift action_55
action_89 (30) = happyShift action_56
action_89 (31) = happyShift action_57
action_89 (32) = happyShift action_58
action_89 (33) = happyShift action_59
action_89 (34) = happyShift action_60
action_89 (35) = happyShift action_61
action_89 (36) = happyShift action_62
action_89 (37) = happyShift action_34
action_89 (38) = happyShift action_63
action_89 (39) = happyShift action_35
action_89 (40) = happyShift action_36
action_89 (42) = happyShift action_37
action_89 (52) = happyShift action_38
action_89 (53) = happyShift action_39
action_89 (55) = happyShift action_40
action_89 (8) = happyGoto action_48
action_89 (9) = happyGoto action_29
action_89 (11) = happyGoto action_49
action_89 _ = happyReduce_18

action_90 (17) = happyShift action_95
action_90 _ = happyFail

action_91 (12) = happyShift action_30
action_91 (14) = happyShift action_31
action_91 (16) = happyShift action_32
action_91 (17) = happyShift action_94
action_91 (22) = happyShift action_33
action_91 (24) = happyShift action_50
action_91 (25) = happyShift action_51
action_91 (26) = happyShift action_52
action_91 (27) = happyShift action_53
action_91 (28) = happyShift action_54
action_91 (29) = happyShift action_55
action_91 (30) = happyShift action_56
action_91 (31) = happyShift action_57
action_91 (32) = happyShift action_58
action_91 (33) = happyShift action_59
action_91 (34) = happyShift action_60
action_91 (35) = happyShift action_61
action_91 (36) = happyShift action_62
action_91 (37) = happyShift action_34
action_91 (38) = happyShift action_63
action_91 (39) = happyShift action_35
action_91 (40) = happyShift action_36
action_91 (42) = happyShift action_37
action_91 (52) = happyShift action_38
action_91 (53) = happyShift action_39
action_91 (55) = happyShift action_40
action_91 (8) = happyGoto action_48
action_91 (9) = happyGoto action_29
action_91 (11) = happyGoto action_49
action_91 _ = happyFail

action_92 (12) = happyShift action_30
action_92 (14) = happyShift action_31
action_92 (15) = happyShift action_77
action_92 (16) = happyShift action_32
action_92 (19) = happyShift action_78
action_92 (22) = happyShift action_33
action_92 (24) = happyShift action_50
action_92 (25) = happyShift action_51
action_92 (26) = happyShift action_52
action_92 (27) = happyShift action_53
action_92 (28) = happyShift action_54
action_92 (29) = happyShift action_55
action_92 (30) = happyShift action_56
action_92 (31) = happyShift action_57
action_92 (32) = happyShift action_58
action_92 (33) = happyShift action_59
action_92 (34) = happyShift action_60
action_92 (35) = happyShift action_61
action_92 (36) = happyShift action_62
action_92 (37) = happyShift action_34
action_92 (38) = happyShift action_63
action_92 (39) = happyShift action_35
action_92 (40) = happyShift action_36
action_92 (42) = happyShift action_37
action_92 (52) = happyShift action_38
action_92 (53) = happyShift action_39
action_92 (55) = happyShift action_40
action_92 (8) = happyGoto action_48
action_92 (9) = happyGoto action_29
action_92 (10) = happyGoto action_93
action_92 (11) = happyGoto action_49
action_92 _ = happyFail

action_93 _ = happyReduce_33

action_94 _ = happyReduce_28

action_95 _ = happyReduce_27

action_96 (12) = happyShift action_30
action_96 (14) = happyShift action_31
action_96 (16) = happyShift action_32
action_96 (22) = happyShift action_33
action_96 (37) = happyShift action_34
action_96 (39) = happyShift action_35
action_96 (40) = happyShift action_36
action_96 (42) = happyShift action_37
action_96 (52) = happyShift action_38
action_96 (53) = happyShift action_39
action_96 (55) = happyShift action_40
action_96 (8) = happyGoto action_99
action_96 (9) = happyGoto action_29
action_96 _ = happyFail

action_97 (12) = happyShift action_30
action_97 (14) = happyShift action_31
action_97 (16) = happyShift action_32
action_97 (22) = happyShift action_33
action_97 (37) = happyShift action_34
action_97 (39) = happyShift action_35
action_97 (40) = happyShift action_36
action_97 (42) = happyShift action_37
action_97 (52) = happyShift action_38
action_97 (53) = happyShift action_39
action_97 (55) = happyShift action_40
action_97 (8) = happyGoto action_98
action_97 (9) = happyGoto action_29
action_97 _ = happyFail

action_98 (12) = happyShift action_30
action_98 (14) = happyShift action_31
action_98 (16) = happyShift action_32
action_98 (22) = happyShift action_33
action_98 (24) = happyShift action_50
action_98 (25) = happyShift action_51
action_98 (26) = happyShift action_52
action_98 (27) = happyShift action_53
action_98 (28) = happyShift action_54
action_98 (29) = happyShift action_55
action_98 (30) = happyShift action_56
action_98 (31) = happyShift action_57
action_98 (32) = happyShift action_58
action_98 (33) = happyShift action_59
action_98 (34) = happyShift action_60
action_98 (35) = happyShift action_61
action_98 (36) = happyShift action_62
action_98 (37) = happyShift action_34
action_98 (38) = happyShift action_63
action_98 (39) = happyShift action_35
action_98 (40) = happyShift action_36
action_98 (42) = happyShift action_37
action_98 (52) = happyShift action_38
action_98 (53) = happyShift action_39
action_98 (55) = happyShift action_40
action_98 (8) = happyGoto action_48
action_98 (9) = happyGoto action_29
action_98 (11) = happyGoto action_49
action_98 _ = happyReduce_21

action_99 (12) = happyShift action_30
action_99 (14) = happyShift action_31
action_99 (16) = happyShift action_32
action_99 (22) = happyShift action_33
action_99 (24) = happyShift action_50
action_99 (25) = happyShift action_51
action_99 (26) = happyShift action_52
action_99 (27) = happyShift action_53
action_99 (28) = happyShift action_54
action_99 (29) = happyShift action_55
action_99 (30) = happyShift action_56
action_99 (31) = happyShift action_57
action_99 (32) = happyShift action_58
action_99 (33) = happyShift action_59
action_99 (34) = happyShift action_60
action_99 (35) = happyShift action_61
action_99 (36) = happyShift action_62
action_99 (37) = happyShift action_34
action_99 (38) = happyShift action_63
action_99 (39) = happyShift action_35
action_99 (40) = happyShift action_36
action_99 (42) = happyShift action_37
action_99 (52) = happyShift action_38
action_99 (53) = happyShift action_39
action_99 (55) = happyShift action_40
action_99 (8) = happyGoto action_48
action_99 (9) = happyGoto action_29
action_99 (11) = happyGoto action_49
action_99 _ = happyReduce_17

happyReduce_1 = happyReduce 6 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	(HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	(HappyTerminal (TokenIdLC happy_var_2)) `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 ((makeDecl happy_var_1 happy_var_2 happy_var_3 happy_var_5) : happy_var_6
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 5 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	(HappyTerminal (TokenIdLC happy_var_2)) `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (makeDecl happy_var_1 happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_3)
	_
	_
	 =  HappyAbsSyn5
		 (Just happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_0  5 happyReduction_4
happyReduction_4  =  HappyAbsSyn5
		 (Nothing
	)

happyReduce_5 = happySpecReduce_2  6 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_2)
	(HappyTerminal (TokenIdLC happy_var_1))
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_2
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_0  6 happyReduction_6
happyReduction_6  =  HappyAbsSyn6
		 ([]
	)

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn7
		 (TInt
	)

happyReduce_8 = happySpecReduce_1  7 happyReduction_8
happyReduction_8 _
	 =  HappyAbsSyn7
		 (TBool
	)

happyReduce_9 = happySpecReduce_1  7 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn7
		 (TChar
	)

happyReduce_10 = happySpecReduce_1  7 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn7
		 (TStr
	)

happyReduce_11 = happySpecReduce_3  7 happyReduction_11
happyReduction_11 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (TList happy_var_2
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 5 7 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (TSum happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 5 7 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (TProd happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_3  7 happyReduction_14
happyReduction_14 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (TFunc happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  7 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  7 happyReduction_16
happyReduction_16 (HappyTerminal (TokenIdLC happy_var_1))
	 =  HappyAbsSyn7
		 (TVar happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happyReduce 6 8 happyReduction_17
happyReduction_17 ((HappyAbsSyn8  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdLC happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (App (AbsInf happy_var_2 happy_var_6) happy_var_4
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 4 8 happyReduction_18
happyReduction_18 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenIdLC happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (AbsInf happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_2  8 happyReduction_19
happyReduction_19 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (App happy_var_1 happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  8 happyReduction_20
happyReduction_20 (HappyTerminal (TokenIdLC happy_var_1))
	 =  HappyAbsSyn8
		 (Var happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happyReduce 6 8 happyReduction_21
happyReduction_21 ((HappyAbsSyn8  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (App (App (App (Operation Cond) happy_var_2) happy_var_4) happy_var_6
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_3  8 happyReduction_22
happyReduction_22 (HappyAbsSyn8  happy_var_3)
	(HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (App (App happy_var_2 happy_var_1) happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  8 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn8
		 (Operation IsZ
	)

happyReduce_24 = happySpecReduce_1  8 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn8
		 (Operation Not
	)

happyReduce_25 = happySpecReduce_1  8 happyReduction_25
happyReduction_25 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  8 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happyReduce 5 8 happyReduction_27
happyReduction_27 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (App (Operation InjL) happy_var_2
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 5 8 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (App (Operation InjR) happy_var_4
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_1  8 happyReduction_29
happyReduction_29 (HappyTerminal (TokenInt  happy_var_1))
	 =  HappyAbsSyn8
		 (Constant (IntVal happy_var_1)
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  8 happyReduction_30
happyReduction_30 (HappyTerminal (TokenBool happy_var_1))
	 =  HappyAbsSyn8
		 (Constant (BoolVal happy_var_1)
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  9 happyReduction_31
happyReduction_31 _
	_
	 =  HappyAbsSyn8
		 (Operation Empty
	)

happyReduce_32 = happySpecReduce_3  9 happyReduction_32
happyReduction_32 (HappyAbsSyn8  happy_var_3)
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (App (App (Operation Cons) happy_var_2) happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  10 happyReduction_33
happyReduction_33 (HappyAbsSyn8  happy_var_3)
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (App (App (Operation Cons) happy_var_2) happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  10 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn8
		 (Operation Empty
	)

happyReduce_35 = happySpecReduce_1  11 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn8
		 (Operation Add
	)

happyReduce_36 = happySpecReduce_1  11 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn8
		 (Operation Sub
	)

happyReduce_37 = happySpecReduce_1  11 happyReduction_37
happyReduction_37 _
	 =  HappyAbsSyn8
		 (Operation Mul
	)

happyReduce_38 = happySpecReduce_1  11 happyReduction_38
happyReduction_38 _
	 =  HappyAbsSyn8
		 (Operation Div
	)

happyReduce_39 = happySpecReduce_1  11 happyReduction_39
happyReduction_39 _
	 =  HappyAbsSyn8
		 (Operation Mod
	)

happyReduce_40 = happySpecReduce_1  11 happyReduction_40
happyReduction_40 _
	 =  HappyAbsSyn8
		 (Operation And
	)

happyReduce_41 = happySpecReduce_1  11 happyReduction_41
happyReduction_41 _
	 =  HappyAbsSyn8
		 (Operation Or
	)

happyReduce_42 = happySpecReduce_1  11 happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn8
		 (Operation Xor
	)

happyReduce_43 = happySpecReduce_1  11 happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn8
		 (Operation Lss
	)

happyReduce_44 = happySpecReduce_1  11 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn8
		 (Operation LsE
	)

happyReduce_45 = happySpecReduce_1  11 happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn8
		 (Operation Equ
	)

happyReduce_46 = happySpecReduce_1  11 happyReduction_46
happyReduction_46 _
	 =  HappyAbsSyn8
		 (Operation NEq
	)

happyReduce_47 = happySpecReduce_1  11 happyReduction_47
happyReduction_47 _
	 =  HappyAbsSyn8
		 (Operation Gtr
	)

happyReduce_48 = happySpecReduce_1  11 happyReduction_48
happyReduction_48 _
	 =  HappyAbsSyn8
		 (Operation GtE
	)

happyNewToken action sts stk [] =
	action 57 57 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenOpenBr -> cont 12;
	TokenClosBr -> cont 13;
	TokenOpenSq -> cont 14;
	TokenClosSq -> cont 15;
	TokenOpenCr -> cont 16;
	TokenClosCr -> cont 17;
	TokenEquals -> cont 18;
	TokenComma -> cont 19;
	TokenColon -> cont 20;
	TokenUndrSc -> cont 21;
	TokenLambda -> cont 22;
	TokenDot -> cont 23;
	TokenAdd -> cont 24;
	TokenSub -> cont 25;
	TokenMul -> cont 26;
	TokenDiv -> cont 27;
	TokenMod -> cont 28;
	TokenEqEq -> cont 29;
	TokenLsEq -> cont 30;
	TokenLess -> cont 31;
	TokenGtEq -> cont 32;
	TokenGrtr -> cont 33;
	TokenNoEq -> cont 34;
	TokenAnd -> cont 35;
	TokenOr -> cont 36;
	TokenNot -> cont 37;
	TokenXor -> cont 38;
	TokenIsZero -> cont 39;
	TokenLet -> cont 40;
	TokenIn -> cont 41;
	TokenIf -> cont 42;
	TokenThen -> cont 43;
	TokenElse -> cont 44;
	TokenTyInt -> cont 45;
	TokenTyBool -> cont 46;
	TokenTyChar -> cont 47;
	TokenTyStr -> cont 48;
	TokenTySum -> cont 49;
	TokenTyProd -> cont 50;
	TokenTyArrw -> cont 51;
	TokenInt  happy_dollar_dollar -> cont 52;
	TokenBool happy_dollar_dollar -> cont 53;
	TokenStr  happy_dollar_dollar -> cont 54;
	TokenIdLC happy_dollar_dollar -> cont 55;
	TokenIdUC happy_dollar_dollar -> cont 56;
	_ -> happyError' (tk:tks)
	}

happyError_ 57 tk tks = happyError' tks
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


-- A Decl object carries data that is to be converted into a function
data Decl = Decl {      -- A standard function declaration.
    tyDec :: Maybe Type -- The user-specified type, which we will check, but is
                        -- not necessarily present.
    name  :: String,    -- The name of the function.
    body  :: TypedExp,  -- The body of the function.
} deriving (Show, Eq)

makeDecl :: Maybe Type -> String -> [String] -> TypedExp -> Decl
makeDecl t n []   x = Decl t n x
makeDecl t n args x = makeDecl t n (init args) (AbsInf (last args) x)

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
