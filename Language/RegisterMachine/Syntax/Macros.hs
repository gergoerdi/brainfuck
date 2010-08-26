module Language.RegisterMachine.Syntax.Macros where

import Language.RegisterMachine.Syntax

type MacroName = String
data MStmt reg label = RegStmt (Stmt reg label)
                     | MacroCall MacroName [reg]
                     deriving Show

data MacroDirective reg label = MStmt (MStmt reg label)
                              | MLabel label
                              deriving Show

data MacroLabel label = GenSymLabel label
                      | GlobalLabel label
                      deriving Show

data Macro reg label = Macro MacroName [reg] [MacroDirective reg (MacroLabel label)] deriving Show
data MacroProgram = MacroProgram [Macro Reg Label] [MacroDirective Reg (MacroLabel Label)] deriving Show
