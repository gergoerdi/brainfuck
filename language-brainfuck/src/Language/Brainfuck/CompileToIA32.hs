{-# LANGUAGE DoRec, GeneralizedNewtypeDeriving #-}
module Language.Brainfuck.CompileToIA32 (compile) where

import qualified Language.Brainfuck.Syntax as BF
import Language.IA32.Syntax
    
import Control.Monad.RWS

newtype Compiler a = Compiler { runCompiler :: RWS () [Directive] Label a } deriving (Monad, MonadFix)

compile :: [BF.Stmt] ->  Program
compile program = Program $ snd $ execRWS (runCompiler (compileMain program)) () (Label 0)

label :: Compiler Label
label = Compiler $ do
  lbl <- get
  tell [LabelDef lbl]
  modify succ
  return lbl

tellOp :: [Op] -> Compiler ()         
tellOp = Compiler . tell . map Op

(ptr, dat) = (Reg reg, Deref reg)
  where reg = EBP

compileMain program = do
  tellOp [Mov ptr (Macro "BUF")]
  mapM_ compileStep $ group program
  tellOp [Mov (Reg EAX) (Imm 1),
          Mov (Reg EBX) (Imm 0),
          Int80]
    
group = foldr f []
  where f x (y@(x', n):ys) | x ≈ x'     = (x', n+1):ys
                           | otherwise  = (x, 1):y:ys
        f x []                          = [(x, 1)]
        
        BF.IncPtr  ≈ BF.IncPtr = True
        BF.DecPtr  ≈ BF.DecPtr = True
        BF.IncData ≈ BF.IncData = True
        BF.DecData ≈ BF.DecData = True
        _          ≈ _          = False

arith 1    arg = Inc arg
arith (-1) arg = Dec arg
arith n    arg | n > 0 = Add arg (Imm $ fromInteger n)
               | n < 0 = Sub arg (Imm $ fromInteger (abs n))

compileStep (BF.IncPtr, n) = tellOp [arith n ptr]
compileStep (BF.DecPtr, n) = tellOp [arith (-n) ptr]
compileStep (BF.IncData, n) = tellOp [arith n dat]
compileStep (BF.DecData, n) = tellOp [arith (-n) dat]
compileStep (BF.Output, 1) = tellOp [Mov (Reg EDX) (Imm 1),
                                     Mov (Reg ECX) (Target ptr),
                                     Mov (Reg EBX) (Imm 1),
                                     Mov (Reg EAX) (Imm 4),
                                     Int80]
compileStep (BF.While prog, 1) = do
  rec
    l <- label
    tellOp [Cmp (Target dat) (Imm 0),
            JmpZero l']
    mapM_ compileStep $ group prog
    tellOp [Jmp l]
    l' <- label
  return ()
