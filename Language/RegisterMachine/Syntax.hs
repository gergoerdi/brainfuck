module Language.RegisterMachine.Syntax where

data Stmt reg label = Inc reg
                    | Dec reg
                    | Clr reg
                    | Output reg
                    | Input reg
                    | Jmp label
                    | Jz reg label
          deriving Show

type Reg = String
type Label = String

data Directive reg label = Stmt (Stmt reg label)
                         | Label label
                         deriving Show
type SourceProgram = [Directive Reg Label]
    
