module Language.RegisterMachine.Syntax where

data Stmt reg label = Inc reg
                    | Dec reg
                    | Clr reg
                    | Output reg
                    | Input reg
                    | Jmp label
                    | Jz reg label
          deriving Show

type RegName = String
type Label = String
    

type Reg = Int   
type LineNum = Int

data SourceDirective = Stmt (Stmt RegName Label)
                     | Label Label
                     deriving Show
type SourceProgram = [SourceDirective]

type FlatProgram reg = [(Int, Stmt reg LineNum)]
    
