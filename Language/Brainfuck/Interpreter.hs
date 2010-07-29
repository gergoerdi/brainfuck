module Language.Brainfuck.Interpreter (run) where

import Language.Brainfuck.Syntax
    
import Data.Char (ord, chr)
import Data.Word (Word8)
import Control.Monad (foldM)
import Data.Function (fix)
    
type Cell = Word8
data Memory = M [Cell] [Cell]            

memory = M [] (cycle [0])            
            
pred' x | x == minBound = maxBound
        | otherwise     = pred x

succ' x | x == maxBound = minBound
        | otherwise     = succ x
                      
evalStep :: Stmt -> Memory -> IO Memory
evalStep IncPtr    = \   (M ls     (x:rs)) -> return $ M (x:ls) rs
evalStep DecPtr    = \   (M (x:ls) rs)     -> return $ M ls (x:rs)
evalStep IncData   = \   (M ls     (x:rs)) -> return $ M ls (succ' x:rs)
evalStep DecData   = \   (M ls     (x:rs)) -> return $ M ls (pred' x:rs)
evalStep Output    = \ m@(M ls     (x:rs)) -> putChar (chr $ fromIntegral x) >> return m
evalStep Input     = \   (M ls     (_:rs)) -> do c <- readLn
                                                 let x' = fromIntegral $ ord c
                                                 return $ M ls (x':rs)
evalStep (While ps) = fix $ \loop ->
                        \ m@(M ls (x:rs)) -> if x == 0 then return m
                                               else foldl (>>=) (return m) evals >>= loop
  where evals = map evalStep ps
                         
eval :: [Stmt] -> Memory -> IO Memory
eval [] = return
eval (p:ps) = \m -> evalStep p m >>= eval ps

-- eval = flip $ foldM $ flip evalStep

run p = eval p memory       
