{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.IA32.Syntax (Label(..), Reg(..), Value(..), Op(..), Directive(..), Program(..)) where

import Data.Word (Word8)
    
newtype Label = Label Integer deriving (Enum, Show)
    
data Reg = EAX | EBX | ECX | EDX | ESP
         deriving Show

data Value = Imm Word8
           | Reg Reg
           | Deref Reg
           | Macro String
           deriving Show
                  
data Op = Inc Value
        | Dec Value
        | Move Value Value
        | Jump Label
        | Cmp Value Value
        | JumpZero Label
        | Int80
        | Push Reg
        | Pop Reg
        deriving Show                 

data Directive = Op Op
               | LabelDef Label
               deriving Show

newtype Program = Program { getDirectives :: [Directive] }
