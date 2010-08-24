module Language.RegisterMachine.ResolveLabels (resolveLabels) where

import Language.RegisterMachine.Syntax    
    
import Control.Monad.RWS
import Data.Maybe
import Control.Applicative
import Control.Monad
    
import Data.Map (Map)
import qualified Data.Map as Map
    
collectLabel (Stmt _) = modify succ
collectLabel (Label l) = do
  lineNum <- get
  tell $ Map.singleton l lineNum

collectLabels prog = let (_, _, labelMap) = runRWS (mapM_ collectLabel prog) () 0
                     in labelMap  
       
resolve l = asks $ fromJust . Map.lookup l

resolveStmt (Jmp l)  = Jmp <$> resolve l
resolveStmt (Jz r l) = Jz r <$> resolve l
resolveStmt (Inc r) = return $ Inc r
resolveStmt (Dec r) = return $ Dec r
resolveStmt (Clr r) = return $ Clr r

resolveDirective (Stmt s) = do s' <- resolveStmt s
                               tell [s']                                    
resolveDirective (Label _) = return ()
                      
resolveLabels :: SourceProgram -> [(LineNum, Stmt RegName LineNum)]
resolveLabels prog = zip [0..] prog'
    where labels = collectLabels prog
          (_, _, prog') = runRWS (mapM resolveDirective prog) labels ()
