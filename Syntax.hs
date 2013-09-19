-- This module defines the abstract syntax of the language

module Syntax where

data Program = String Expression Program
             | EndProgram
  deriving Show

data Expression = Int
                | Expression Op Expression
  deriving Show

data Op = Add
        | Sub
        | Mul
        | Div
        | Mod
  deriving Show

eval :: Expression -> Int
eval a Add b = (eval a) + (eval b)
eval a Sub b = (eval a) - (eval b)
eval a Mul b = (eval a) * (eval b)
eval a Div b = div (eval a) (eval b)
eval a Mod b = mod (eval a) (eval b)
eval val = val
