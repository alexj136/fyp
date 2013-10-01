-- This module defines the abstract syntax of the language
module AbstractSyntax where

data Program = Declaration String Expression
             | EndProgram

-- Lambda Expressions need a mechanism to handle naming clashes when replacing
-- variables (bound variables with mathcing names should be renamed
data Expression = Lambda String Expression
                | Name String
                | Application Expression Expression
-- Churchy Expressions:
                | Literal Int
                | Arithmetic Expression Op Expression
                | IfThenElse Expression Expression Expression
    deriving Show

data Op = Add | Sub | Mul | Div | Mod
    deriving Show
