{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.RegisterMachine.CompileToLoop where

import Prelude hiding (mapM)
import Data.Traversable (mapM)        
import Control.Monad.State hiding (mapM)
import Control.Monad.RWS hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Data.List (nubBy)    

import qualified Language.RegisterMachine.Syntax as R
import Language.RegisterMachine.CompileToLoop.Partitions
import Language.RegisterMachine.CompileToLoop.Labeller
import qualified Language.Loop.Syntax as L


data Var reg = Reg reg
             | Pc Int
             | B
             | Z
             | NZ
             deriving Show

toLoop :: R.SourceProgram -> [L.Stmt Int]
toLoop prog = let (_, _, prog'') = runRWS compileProg undefined ()
              in layout prog''
    where parts = partitions prog
          len = length parts
          last = Pc len

          compileProg = censor toMainLoop $ mapM (uncurry compileStmts) parts
              where toMainLoop body = [L.Dec last, L.Inc (Pc 0), L.While last body]

          layout prog = let (prog', _) = runLabeller layoutProg [4 + len..]
                        in prog'
              where layoutProg = mapM (mapM layoutVar) prog

layoutVar B = return 0
layoutVar Z = return 1
layoutVar NZ = return 2
layoutVar (Pc n) = return $ 3 + n
layoutVar (Reg r) = ensure r
                                              
emit = tell
nextLabel = asks succ
next = do pc <- liftM Pc nextLabel
          emit $ [L.Inc pc]

compileStmts l ss = do
  local (const l) $ censor toCase $ mapM compile ss
      where toCase body = [L.While pc $ (L.Dec pc):body]
            pc = Pc l

compile (R.Inc r)     = emit [L.Inc (Reg r)]
compile (R.Dec r)     = emit [L.Dec (Reg r)]
compile (R.Clr r)     = emit [L.Clr (Reg r)]
compile (R.Mov r1 r2) = emit [L.Clr (Reg r1),
                              L.While (Reg r2) [
                                L.Dec (Reg r2),
                                L.Inc B],
                              L.While B [
                                L.Dec B,
                                L.Inc (Reg r1),
                                L.Inc (Reg r2)]]
compile (R.Output r)  = emit [L.Output (Reg r)]
compile (R.Input r)   = emit [L.Input (Reg r)]
compile (R.Jmp l)     = emit [L.Inc (Pc l)]
compile (R.Jz r l)    = do
  pcNonZero <- liftM Pc nextLabel
  let pcZero = Pc l              
  emit $ [L.Inc Z,
          L.Inc NZ,
                       
          L.While (Reg r) [
            L.Dec (Reg r),
            L.Inc B,
            L.Clr Z],
          L.While B [
            L.Dec B,
            L.Inc (Reg r)],
          
          L.While Z [
            L.Dec Z,
            L.Dec NZ,
            L.Inc pcZero],
          L.While NZ [
            L.Dec NZ,
            L.Inc pcNonZero]]
        
p = [
 R.Stmt $ R.Inc "x",
 R.Label "foo",
 R.Label "bar",
 R.Stmt $ R.Inc "y",
 R.Stmt $ R.Dec "x",
 R.Stmt $ R.Jz "x" "bar",
 R.Stmt $ R.Inc "z",
 R.Stmt $ R.Jmp "foo",
 R.Stmt $ R.Inc "z"]
