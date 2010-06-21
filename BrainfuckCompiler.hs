{-# LANGUAGE DoRec, GeneralizedNewtypeDeriving #-}
module Main where

import Language.Brainfuck.Syntax
import Language.Brainfuck.Parser
import Language.Brainfuck.Interpreter
import Language.Ia32.Syntax
    
import Control.Monad.RWS
import Text.PrettyPrint.HughesPJClass (pPrint)
    
import System.Environment (getArgs)

newtype Compiler a = Compiler { runCompiler :: RWS () [Directive] Label a } deriving (Monad, MonadFix)

compile :: [Stmt] ->  [Directive]
compile program = snd $ execRWS (runCompiler (compileMain program)) () (Label 0)

label :: Compiler Label
label = Compiler $ do
  lbl <- get
  tell [LabelDef lbl]
  modify succ
  return lbl

tellOp :: [Op] -> Compiler ()         
tellOp = Compiler . tell . map Op

compileMain program = do
  tellOp [Move (Reg ESP) (Macro "BUF")]
  mapM_ compileStep program
  tellOp [Move (Reg EAX) (Imm 1),
          Move (Reg EBX) (Imm 0),
          Int80]

compileStep IncPtr = tellOp [Inc (Reg ESP)]
compileStep DecPtr = tellOp [Dec (Reg ESP)]
compileStep IncData = tellOp [Inc (Deref ESP)]
compileStep DecData = tellOp [Dec (Deref ESP)]
compileStep Output = tellOp [
                      Move (Reg EDX) (Imm 1),
                      Move (Reg ECX) (Reg ESP),
                      Move (Reg EBX) (Imm 1),
                      Move (Reg EAX) (Imm 4),
                      Int80]
compileStep (While prog) = do
  rec
    l <- label
    tellOp [Cmp (Deref ESP) (Imm 0),
            JumpZero l']
    mapM compileStep prog
    tellOp [Jump l]
    l' <- label
  return ()
  
         
main = do args <- getArgs
          case args of
            [filename] -> do parseRes <- parseBrainFuck filename
                             case parseRes of
                               Left err -> error (show err)
                               Right prog -> do let compiled = compile prog
                                                print $ pPrint $ Program compiled
            _ -> error "Usage: brainfuck filename.bf"
