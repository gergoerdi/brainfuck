module Language.BrainFuck.Interpreter (run) where

import Language.BrainFuck.Syntax
    
import Control.Monad (foldM)
import Data.Char (ord, chr)
import Data.Word (Word8)
import Control.Monad.State
    
type Cell = Word8
data Memory = M [Cell] [Cell]            

zipL (M (l:ls) rs)     = M ls     (l:rs)
zipR (M ls     (r:rs)) = M (r:ls) rs

affect f (M ls (r:rs)) = M ls ((f r):rs)
inspect (M ls (r:rs)) = r
                         
eval = mapM_ evalStep

evalStep :: Stmt -> StateT Memory IO ()
evalStep IncPtr = modify zipR
evalStep DecPtr = modify zipL
evalStep IncData = modify (affect succ')
    where succ' x | x == maxBound = minBound
                  | otherwise = succ x
evalStep DecData = modify (affect pred')
    where pred' x | x == minBound = maxBound
                  | otherwise = pred x
evalStep Output = do v <- gets inspect
                     let c = chr $ fromIntegral v
                     lift $ putChar c
evalStep Input = do c <- lift readLn
                    let v = fromIntegral $ ord c
                    modify $ affect $ const v
evalStep (While p) = do r <- gets inspect
                        case r of
                          0 -> return ()
                          _ -> eval p >> evalStep (While p)                                                         

run p = evalStateT (eval p) (M [] (cycle [0]))      
