{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.IA32.Syntax where

import Data.Word (Word8)
    
newtype Label = Label Integer deriving (Enum, Show)
    
data Reg = EAX | EBX | ECX | EDX | ESP | EBP
         deriving Show

data Target = Reg Reg
            | Deref Reg
            deriving Show

data Value = Target Target
           | Imm Word8
           | Macro String
           deriving Show
                  
data Op = Inc Target
        | Dec Target
        | Add Target Value
        | Sub Target Value
        | Mov Target Value
        | Jmp Label
        | Cmp Value Value
        | JmpZero Label
        | Int80
        deriving Show                 

data Directive = Op Op
               | LabelDef Label
               deriving Show

newtype Program = Program { getDirectives :: [Directive] }
