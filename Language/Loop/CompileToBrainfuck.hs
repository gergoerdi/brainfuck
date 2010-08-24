module Language.Loop.CompileToBrainfuck (compile) where

import qualified Language.Loop.Syntax as L
import qualified Language.Brainfuck.Syntax as B

compile :: [L.Stmt Int] -> [B.Stmt]
compile = optimize . compileStmts

compileStmts = concatMap compileStmt

compileStmt (L.Inc k) = at k [B.IncData] 
compileStmt (L.Dec k) = at k [B.DecData]
compileStmt (L.Clr k) = at k [B.While [B.DecData]]
compileStmt (L.While k p) = at k [B.While $ goto (-k) ++ compileStmts p ++ goto k]

goto k | k >= 0     = replicate k B.IncPtr
       | otherwise = replicate (-k) B.DecPtr

at k x = goto k ++ x ++ goto (-k)

optimize :: B.Program -> B.Program
optimize xs = compactify xs []
  where B.IncPtr  ≈ B.DecPtr  = True
        B.DecPtr  ≈ B.IncPtr  = True
        B.IncData ≈ B.DecData = True
        B.DecData ≈ B.IncData = True
        _         ≈ _         = False

        compactify (x:xs) (y:ys)       | x ≈ y = compactify xs ys
        compactify ((B.While p):xs) ys         = compactify xs ((B.While $ optimize p):ys)
        compactify (x:xs) ys                   = compactify xs (x:ys)
        compactify []     ys                   = reverse ys
