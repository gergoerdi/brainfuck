module Language.RegisterMachine.Syntax.Macros where

import Language.RegisterMachine.Syntax

type Symbol = String

type RegisterSym = Symbol

data LabelSym = Global Symbol
              | GenSym Symbol
              deriving Show
                       
data Arg = Symbol Symbol
         | Int Int
         deriving Show

type PrimitiveStmt = Stmt RegisterSym LabelSym

data MacroStmt = PrimitiveStmt PrimitiveStmt
               | Add RegisterSym Arg
               | MacroCall Symbol [Arg]
               deriving Show
                 
data MacroDirective = MacroStmt MacroStmt                 
                    | MacroLabel LabelSym
                    deriving Show
                 
data Macro = Macro Symbol [Symbol] [MacroDirective] 
           deriving Show
                    
data MacroProgram = MacroProgram [Macro] [MacroDirective]
                  deriving Show

processMacros :: MacroProgram -> SourceProgram
processMacros = undefined


