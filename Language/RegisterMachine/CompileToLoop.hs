{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.RegisterMachine.CompileToLoop where

import Prelude hiding (mapM)
import Data.Traversable (mapM)        
import Control.Monad.RWS hiding (mapM)
    
import qualified Language.RegisterMachine.Syntax as R
import Language.RegisterMachine.ResolveLabels
import qualified Language.Loop.Syntax as L

import Data.Map (Map)
import qualified Data.Map as Map
    
data Var reg = Reg reg
             | Pc Int
             | B
             | Z
             | NZ
             deriving Show

-- compileToLoop :: R.SourceProgram -> [L.Stmt Integer]
compileToLoop prog = let (_, _, prog'') = runRWS compileProg undefined ()
                     in layout prog''
    where prog' = resolveLabels prog
          len = length prog'
          last = Pc len

          compileProg = censor toMainLoop $ mapM (uncurry compileStmt) prog'
              where toMainLoop body = [L.Dec last, L.While last body]

          layout prog = let (prog', _, ()) = runRWS layoutProg len Map.empty
                        in prog'
              where layoutProg = mapM (mapM layoutVar) prog
                                      
          -- -- layout = id
          -- layout B = 0
          -- layout Z = 1
          -- layout NZ = 2
          -- layout (Pc n) = 3 + n
          -- layout (Reg r) = 3 + len + r

layoutVar B = return 0
layoutVar Z = return 1
layoutVar NZ = return 2
layoutVar (Pc n) = return $ 3 + n
layoutVar (Reg r) = do lookup <- gets $ Map.lookup r
                       case lookup of
                         Just k -> return k
                         Nothing -> do len <- ask
                                       count <- gets Map.size
                                       let k = 4 + len + count
                                       modify $ Map.insert r k
                                       return k
                                              
emit = tell
nextLabel = asks succ
next = do pc <- liftM Pc nextLabel
          emit $ [L.Inc pc]

compileStmt l s = do
  local (const l) $ censor toCase $ compile s
      where toCase body = [L.While pc $ (L.Dec pc):body]
            pc = Pc l

compile (R.Inc r) = do emit [L.Inc (Reg r)]
                       next

compile (R.Dec r) = do emit [L.Dec (Reg r)]
                       next

compile (R.Clr r) = do emit [L.Clr (Reg r)]
                       next

compile (R.Jmp l) = emit [L.Inc (Pc l)]
compile (R.Jz r l) = do
  pcNonZero <- liftM Pc nextLabel
  let pcZero = Pc l              
  emit $ [L.Inc Z,
          L.Inc NZ,
                       
           -- L.Clr B,                       
          L.While (Reg r)
               [L.Dec (Reg r),
                L.Inc B,
                L.Clr NZ],
          L.While B
               [L.Dec B,
                L.Inc (Reg r)],
                           
          L.While NZ
               [L.Dec NZ,
                L.Dec Z,
                L.Inc pcNonZero],
          L.While Z
               [L.Dec Z,
                L.Inc pcZero]]
                            
