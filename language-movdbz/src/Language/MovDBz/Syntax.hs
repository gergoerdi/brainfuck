module Language.MovDBz.Syntax where

import Data.Bifunctor

data Stmt reg label = MOVDBZ reg reg label label
                    | PRINT reg label
                    | HALT
                    deriving Show

instance Bifunctor Stmt where
    bimap f g (MOVDBZ from to ifZero els) = MOVDBZ (f from) (f to) (g ifZero) (g els)
    bimap f g (PRINT reg label) = PRINT (f reg) (g label)
    bimap f g HALT = HALT

type Program reg label = [(label, Stmt reg label)]
