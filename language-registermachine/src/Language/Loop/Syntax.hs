{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Language.Loop.Syntax where

import Data.Foldable
import Data.Traversable

data Stmt reg = Inc reg
              | Dec reg
              | Clr reg
              | Output reg
              | Input reg
              | While reg [Stmt reg]
              deriving (Show, Functor, Foldable, Traversable)
