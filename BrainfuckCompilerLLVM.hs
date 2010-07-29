{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}
module Main where

import Prelude hiding (print)    
    
import Language.Brainfuck.Syntax
import Language.Brainfuck.Parser
import Language.Brainfuck.Interpreter
    
import System.Environment (getArgs)
import Control.Applicative
import Data.Word

import LLVM.Core
import LLVM.Util.Optimize
import LLVM.ExecutionEngine    

import Control.Monad.RWS

data R = R { buf :: Value (Ptr Word8),
             print :: Function (Word8 -> IO ())}
data S = S { idx :: Value Word32 }
      
type Compiler r a = RWST R () S (CodeGenFunction r) a    

putIdx idx = modify $ \s -> s{idx = idx}
    
compile :: Stmt -> Compiler r ()    
compile IncPtr = do
  idx <- gets idx
  (lift $ add idx (valueOf (1 :: Word32))) >>= putIdx
  return ()

compile DecPtr = do
  idx <- gets idx
  (lift $ sub idx (valueOf (1 :: Word32))) >>= putIdx
  return ()
      
compile IncData = do
  withCell $ \v -> add v (valueOf (1 :: Word8))
                           
compile DecData = do
  withCell $ \v -> sub v (valueOf (1 :: Word8))
                           
compile Output = do
  v <- readCell

  print <- asks print
  lift $ call print v          
  return ()
         
compile (While prog) = do
  buf <- asks buf
  i <- gets idx

  loop <- lift $ newBasicBlock
  body <- lift $ newBasicBlock
  end <- lift $ newBasicBlock
         
  start <- lift $ getCurrentBasicBlock
  lift $ br loop
         
  lift $ defineBasicBlock loop
  i' <- lift $ phi [(i, start)]
  ptr <- lift $ getElementPtr buf (i', ())
  v <- lift $ load ptr
  finished <- lift $ icmp IntEQ v (valueOf (0 :: Word8))
  lift $ condBr finished end body

  lift $ defineBasicBlock body
  putIdx i'
  mapM_ compile prog
  i'' <- gets idx
  lift $ addPhiInputs i' [(i'', body)]
  lift $ br loop
         
  lift $ defineBasicBlock end
  putIdx i''
  return ()

-- compile _ = return ()

withCell f = do
  v <- readCell
  v' <- lift $ f v
  writeCell v'
  return ()

calcPtr = do
  buf <- asks buf
  idx <- gets idx
  lift $ getElementPtr buf (idx, ())
         
readCell = do
  ptr <- calcPtr
  lift $ load ptr

writeCell v = do
  ptr <- calcPtr
  lift $ store v ptr
         
compileMain prog = do
  putchar <- newNamedFunction ExternalLinkage "putchar" :: CodeGenModule (Function (Word32 -> IO Word32))

  print :: Function (Word8 -> IO ()) <- createFunction InternalLinkage $ \v -> do
    v' <- zext v
    call putchar v'
    ret ()
            
  main :: Function (IO ()) <- createNamedFunction ExternalLinkage "main" $ do
    buf <- arrayAlloca (30000 :: Word32) :: CodeGenFunction r (Value (Ptr Word8))

    do
      start <- getCurrentBasicBlock
      loop <- newBasicBlock
      end <- newBasicBlock

      br loop      
      defineBasicBlock loop      
      i <- phi [(valueOf (0 :: Word32), start)]
      ptr <- getElementPtr buf (i, ())
      store (valueOf (0 :: Word8)) ptr
            
      i' <- add i (valueOf (1 :: Word32))
      addPhiInputs i [(i', loop)]
      finished <- icmp IntEQ i' (valueOf (30000 :: Word32))
      condBr finished end loop

      defineBasicBlock end

    idx <- return $ valueOf (0 :: Word32)
    runRWST (mapM compile prog) R{buf=buf, print=print} S{idx=idx} 
    ret ()
        
  return main

main = do args <- getArgs
          case args of
            [filename] -> do parseRes <- parseBrainFuck filename
                             case parseRes of
                               Left err -> error (show err)
                               Right prog -> do mod <- bf prog
                                                writeBitcodeToFile "bf.bc" mod
            _ -> error "Usage: brainfuck filename.bf"

bf prog = do
  mod <- newNamedModule "brainfuck"
  main <- defineModule mod (compileMain prog)
  dumpValue main
  return mod
