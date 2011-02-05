{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude hiding (print)    
    
import Language.Brainfuck.Syntax
import Language.Brainfuck.Parser
    
import System.Environment (getArgs)
import System (getProgName)
import System.Path (splitExt)
import System.Path.NameManip (split_path)

import Control.Monad.State
import Data.Word

import LLVM.Core

memSize :: Word32
memSize = 30000

compileToModule stmts = do
  (putchar :: Function (Word32 -> IO Word32)) <- newNamedFunction ExternalLinkage "putchar"
  (getchar :: Function (IO Word32)) <- newNamedFunction ExternalLinkage "getchar"
  (main :: Function (IO ())) <- newNamedFunction ExternalLinkage "main"    
    
  defineFunction main $ do
    mem :: Value (Ptr Word8) <- arrayAlloca memSize    
                                 
    let read = do 
          index <- get
          ptr <- lift $ getElementPtr mem (index, ())
          lift $ load ptr            
        set x = do 
          index <- get
          ptr <- lift $ getElementPtr mem (index, ())
          lift $ store x ptr
                                 
        compile IncPtr = do
          index <- get            
          put =<< lift (add index (valueOf (1 :: Word32)))
        compile DecPtr = do
          index <- get            
          put =<< lift (sub index (valueOf (1 :: Word32)))
        compile IncData = do
          x <- read
          set =<< lift (add x (valueOf (1 :: Word8)))
        compile DecData = do
          x <- read
          set =<< lift (sub x (valueOf (1 :: Word8)))
        compile Output = do
          x <- read
          x' <- lift $ zext x
          _ <- lift $ call putchar x'
          return ()
        compile Input = do
          x <- lift $ call getchar
          x' <- lift $ trunc x
          set x'
        compile (While stmts) = do
          loopStart <- lift $ newBasicBlock
          loop <- lift $ newBasicBlock
          loopAfter <- lift $ newBasicBlock
          before <- lift $ getCurrentBasicBlock
          lift $ br loopStart
          
          lift $ defineBasicBlock loopStart
          index <- get            
          index' <- lift $ phi [(index, before)]
          put index'
          x <- read
          b <- lift $ icmp IntEQ x (valueOf (0 :: Word8))
          lift $ condBr b loopAfter loop
          
          lift $ defineBasicBlock loop                        
          mapM_ compile stmts
          index'' <- get
          loopEnd <- lift $ getCurrentBasicBlock
          lift $ addPhiInputs index' [(index'', loopEnd)]
          lift $ br loopStart
          
          put index'
          lift $ defineBasicBlock loopAfter
          
    evalStateT (mapM_ compile stmts) (valueOf (0 :: Word32))
    ret ()

compile prog = do
  m <- newModule
  defineModule m $ compileToModule prog
  return m

main = do
  args <- getArgs
  case args of
    [filename] -> do
      parseRes <- parseBrainFuck filename
      case parseRes of
        Left err -> error (show err)
        Right prog -> do
          m <- compile prog
          writeBitcodeToFile outFilename m            
      where
        (dir, filename') = split_path filename
        (basename, ext) = splitExt filename'
        outFilename = (if ext == ".bf" then basename else filename') ++ ".bc"

    _ -> do 
      self <- getProgName
      error $ unwords ["Usage:", self, "filename.bf"]
