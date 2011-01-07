{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.RegisterMachine.Assembler (Assembler, Register(..), inc, dec, clr, mov, output, input, jmp, jz, 
                                           (<~),
                                           label,
                                           assemble)
       where

import Language.RegisterMachine.Syntax
import Control.Monad.RWS

data Register = Named String
              | Auto Int
              deriving (Eq, Ord, Show)

newtype Assembler a = Assembler { runAssembler :: RWS () [Directive Register Int] [Int] a } deriving (Monad, MonadFix)

emitStmt s = Assembler $ tell [Stmt s]

inc            = emitStmt . Inc
dec            = emitStmt . Dec
clr            = emitStmt . Clr
mov rDest rSrc = emitStmt $ Mov rDest rSrc
output         = emitStmt . Output
input          = emitStmt . Input
jmp            = emitStmt . Jmp
jz r l         = emitStmt $ Jz r l

(<~) = mov

label = Assembler $ do
  (l:ls) <- get
  put ls
  tell [Label l]
  return l

assemble :: Assembler a -> (a, [Directive Register Int])
assemble a = (evalRWS . runAssembler) a () [0..]
