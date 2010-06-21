module Language.Brainfuck.Syntax where

data Stmt = IncPtr
          | DecPtr
          | IncData
          | DecData
          | Output
          | Input
          | While Program
          deriving Show
            
type Program = [Stmt]            
