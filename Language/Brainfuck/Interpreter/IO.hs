module Language.Brainfuck.Interpreter.IO(run) where

import Language.Brainfuck.Syntax
    
import Data.Char (ord, chr)
import Data.Word (Word8, Word16)
import Control.Monad.RWS

import Data.Array.IO
    
type Cell = Word8
type Ptr = Word16
type Memory = IOArray Ptr Cell
        
type Interpreter a = RWST Memory () Ptr IO a
    
eval = mapM_ evalStep
       
succ' x | x == maxBound = minBound
        | otherwise     = succ x

pred' x | x == minBound = maxBound
        | otherwise     = pred x

askData :: Interpreter Cell                          
askData = do ptr <- get
             a <- ask
             lift $ readArray a ptr

modifyData :: (Cell -> Cell) -> Interpreter ()                  
modifyData f = do ptr <- get
                  a <- ask
                  v <- lift $ readArray a ptr
                  lift $ writeArray a ptr (f v)
                        
                          
evalStep IncPtr = modify succ
evalStep DecPtr = modify pred
evalStep IncData = modifyData succ'
evalStep DecData = modifyData pred'
evalStep Output = do v <- askData
                     let c = chr $ fromIntegral v
                     lift $ putChar c
evalStep Input = do c <- lift readLn
                    let v' = fromIntegral $ ord c
                    modifyData (const v')
evalStep (While p) = do v <- askData
                        case v of
                          0 -> return ()
                          _ -> eval p >> evalStep (While p)
                   
run p = do memory <- newListArray (minBound, maxBound) (repeat 0)
           execRWST (eval p) memory minBound
           return memory
