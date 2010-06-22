module Language.Brainfuck.Interpreter.IO(run) where

import Language.Brainfuck.Syntax
    
import Data.Char (ord, chr)
import Data.Word (Word8, Word16)
import Data.IORef
import Data.Array.IO
    
type Cell = Word8
type Ptr = Word16
type Memory = IOUArray Ptr Cell
        
succ' x | x == maxBound = minBound
        | otherwise     = succ x

pred' x | x == minBound = maxBound
        | otherwise     = pred x

run p = do memory <- newListArray (minBound, maxBound) (repeat 0) :: IO Memory
           ptr <- newIORef 0
           evalProgram ptr memory
                       
    where evalProgram ptr memory = eval p
              where eval = mapM_ evalStep
                           
                    evalStep IncPtr = modifyIORef ptr succ
                    evalStep DecPtr = modifyIORef ptr pred
                    evalStep IncData = do i <- readIORef ptr
                                          x <- readArray memory i
                                          writeArray memory i (succ' x)
                    evalStep DecData = do i <- readIORef ptr
                                          x <- readArray memory i
                                          writeArray memory i (pred' x)
                    evalStep Output = do i <- readIORef ptr
                                         x <- readArray memory i
                                         let c = chr $ fromIntegral x
                                         putChar c
                    evalStep (While p) = do i <- readIORef ptr
                                            x <- readArray memory i
                                            if x == 0 then return ()
                                               else eval p >> evalStep (While p)
                                         
