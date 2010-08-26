{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.RegisterMachine.CompileToLoop.Partitions (partitions) where

import Language.RegisterMachine.Syntax
import Language.RegisterMachine.CompileToLoop.Labeller

import Control.Monad.State
import Data.Maybe (fromJust)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative    

data LabelCollector = LabelCollector { labelFirst :: Maybe Label,
                                       labels :: Map Label Label }
                      deriving Show
  
getLabelFirst = gets labelFirst
putLabelFirst l = modify $ \s -> s{labelFirst = l}

getLabels = gets labels
addLabel label repr = modify $ \s -> s{labels = Map.insert label repr (labels s)}

collectLabel (Label l) = do
  labelFirst <- getLabelFirst
  case labelFirst of
    Just l' -> addLabel l l'
    Nothing -> do addLabel l l
                  putLabelFirst (Just l)  

collectLabel (Stmt _) = putLabelFirst Nothing

unifyLabels :: SourceProgram -> SourceProgram
unifyLabels prog = compress False $ map unify prog
  where s' = execState (mapM_ collectLabel prog) $ LabelCollector Nothing Map.empty
        lookup l = fromJust $ Map.lookup l $ labels s'                                        

        unify (Label l) = Label $ lookup l
        unify (Stmt s) = Stmt $ unifyStmt s
        
        unifyStmt (Jmp l)  = Jmp (lookup l)
        unifyStmt (Jz r l) = Jz r (lookup l)
        unifyStmt s        = s
        
        compress False ((Label l):xs) = (Label l):compress True xs
        compress True  ((Label _):xs) = compress True xs
        compress _     (x:xs)         = x:compress False xs
        compress _     []             = []

groupLabels :: SourceProgram -> [(Maybe Label, [Stmt Reg Label])]
groupLabels = groupLabels' (Nothing, [])
  where groupLabels' (l, ss) ((Label l'):xs) = (l, reverse ss):groupLabels' (Just l', []) xs
        groupLabels' (l, ss) ((Stmt s):xs)   = groupLabels' (l, s:ss) xs
        groupLabels' (l, ss) []              = [(l, reverse ss)]

prune :: [Stmt Reg Label] -> [Stmt Reg Label]        
prune = takeWhile (not . isJmp)
  where isJmp (Jmp _) = True
        isJmp _       = False
        
cut :: [Stmt Reg Label] -> [[Stmt Reg Label]]
cut = groupBy (\ x y -> not (isJz y))
  where isJz (Jz _ _) = True
        isJz _        = False        
        
partitions :: SourceProgram -> [(Int, [Stmt Reg Int])]        
partitions p = fst $ runLabeller (mapM resolve parts) [0..]
  where parts = concatMap cut' $ map (fmap prune) $ groupLabels $ unifyLabels p 
        cut' (l, ss) = zip (l:repeat Nothing) (cut ss)
        
        resolveLabel Nothing = generate
        resolveLabel (Just l) = ensure l
        
        resolveStmt (Inc r) = return $ Inc r
        resolveStmt (Dec r) = return $ Dec r
        resolveStmt (Clr r) = return $ Clr r
        resolveStmt (Output r) = return $ Output r
        resolveStmt (Input r) = return $ Input r
        resolveStmt (Jmp l) = Jmp <$> ensure l
        resolveStmt (Jz r l) = Jz r <$> ensure l
        
        resolve (l, ss) = do l' <- resolveLabel l
                             ss' <- mapM resolveStmt ss
                             return (l', ss')
        
p = [
 Stmt $ Inc "x",
 Label "foo",
 Label "bar",
 Stmt $ Inc "y",
 Stmt $ Dec "x",
 Stmt $ Jz "x" "bar",
 Stmt $ Inc "z",
 Stmt $ Jmp "foo",
 Stmt $ Inc "z"]
