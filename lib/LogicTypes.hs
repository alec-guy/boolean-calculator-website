{-# LANGUAGE OverloadedStrings #-}

module LogicTypes where 

data Gate a = And (Gate a) (Gate a) 
            | Or (Gate a) (Gate a)
            | If (Gate a) (Gate a)
            | Iff (Gate a) (Gate a)
            | Xor (Gate a) (Gate a)
            | Nand (Gate a) (Gate a)
            | Nor (Gate a) (Gate a)
            | Not (Gate a)
            | Id (Gate a)
            | Wire a 
            deriving (Show, Eq)


