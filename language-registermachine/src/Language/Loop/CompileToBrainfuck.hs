module Language.Loop.CompileToBrainfuck (toBrainfuck) where

import qualified Language.Loop.Syntax as L
import qualified Language.Brainfuck.Syntax as B

toBrainfuck :: [L.Stmt Int] -> [B.Stmt]
toBrainfuck = optimize . compiles

compiles = concatMap compile

compile (L.Inc k)     = at k [B.IncData] 
compile (L.Dec k)     = at k [B.DecData]
compile (L.Clr k)     = at k [B.While [B.DecData]]
compile (L.Output k)  = at k [B.Output]
compile (L.Input k)   = at k [B.Input]
compile (L.While k p) = at k [B.While $ goto (-k) ++ compiles p ++ goto k]

goto k | k >= 0     = replicate k B.IncPtr
       | otherwise = replicate (-k) B.DecPtr

at k x = goto k ++ x ++ goto (-k)

optimize :: B.Program -> B.Program
optimize xs = telescope xs []
  where B.IncPtr  ≈ B.DecPtr  = True
        B.DecPtr  ≈ B.IncPtr  = True
        B.IncData ≈ B.DecData = True
        B.DecData ≈ B.IncData = True
        _         ≈ _         = False

        telescope (x:xs) (y:ys)       | x ≈ y = telescope xs ys
        telescope ((B.While p):xs) ys         = telescope xs ((B.While $ optimize p):ys)
        telescope (x:xs) ys                   = telescope xs (x:ys)
        telescope []     ys                   = reverse ys
