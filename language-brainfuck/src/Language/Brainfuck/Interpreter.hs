module Language.Brainfuck.Interpreter (run) where

import Language.Brainfuck.Syntax
    
import Data.Char (ord, chr)
import Data.Word (Word8)
import Control.Monad (foldM)
import Control.Applicative ((<$>))    

type Cell = Word8
data Memory = M [Cell] [Cell]            

pred' x | x == minBound = maxBound
        | otherwise     = pred x

succ' x | x == maxBound = minBound
        | otherwise     = succ x
                      
evalStep :: Memory -> Stmt -> IO Memory
evalStep   (M ls     (x:rs)) IncPtr  = return $ M (x:ls) rs
evalStep   (M (x:ls) rs)     DecPtr  = return $ M ls (x:rs)
evalStep   (M ls     (x:rs)) IncData = return $ M ls (succ' x:rs)
evalStep   (M ls     (x:rs)) DecData = return $ M ls (pred' x:rs)
evalStep m@(M ls     (x:rs)) Output  = putChar (chr $ fromIntegral x) >> return m
evalStep   (M ls     (_:rs)) Input   = do x <- fromIntegral <$> ord <$> readLn
                                          return $ M ls (x:rs)
evalStep m@(M ls     (x:rs)) p@(While ps) | x == 0 = return m
                                          | otherwise = do m' <- eval m ps 
                                                           evalStep m' p
                         
eval :: Memory -> [Stmt] -> IO Memory
eval = foldM evalStep

run = eval memory
  where memory = M [] (cycle [0])
