{-# LANGUAGE NoMonomorphismRestriction #-}
module Language.RegisterMachine.CompileToLoop.Partitions (partitions) where

import Language.RegisterMachine.Syntax
import Language.RegisterMachine.CompileToLoop.Labeller

import Control.Monad.State
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map

data LabelCollector l = LabelCollector { labelFirst :: Maybe l,
                                         labels :: Map l l }
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

unifyLabels :: (Ord l, Show l) => [Directive r l] -> [Directive r l]
unifyLabels prog = compress False $ map unify prog
  where s' = execState (mapM_ collectLabel prog) $ LabelCollector Nothing Map.empty
        lookup l = case Map.lookup l $ labels s' of
          Nothing -> error $ unwords ["Undefined label", show l]
          Just l' -> l'

        unify (Label l) = Label $ lookup l
        unify (Stmt s) = Stmt $ unifyStmt s
        
        unifyStmt (Jmp l)  = Jmp (lookup l)
        unifyStmt (Jz r l) = Jz r (lookup l)
        unifyStmt s        = s
        
        compress False ((Label l):xs) = (Label l):compress True xs
        compress True  ((Label _):xs) = compress True xs
        compress _     (x:xs)         = x:compress False xs
        compress _     []             = []

groupLabels :: [Directive r l] -> [(Maybe l, [Stmt r l])]
groupLabels = groupLabels' (Nothing, [])
  where groupLabels' (l, ss) ((Label l'):xs) = (l, reverse ss):groupLabels' (Just l', []) xs
        groupLabels' (l, ss) ((Stmt s):xs)   = groupLabels' (l, s:ss) xs
        groupLabels' (l, ss) []              = [(l, reverse ss)]

prune :: [Stmt r l] -> [Stmt r l]
prune = foldr untilJmp []
  where untilJmp s acc | isJmp s   = [s]
                       | otherwise = s:acc
                                     
        isJmp (Jmp _)  = True
        isJmp (Jz _ _) = True
        isJmp _        = False
        
cut :: [Stmt r l] -> [[Stmt r l]]
cut ss = cut' [] ss
  where cut' group (s@(Jz _ _):ss) = (reverse (s:group)):cut' [] ss
        cut' group (s:ss)          = cut' (s:group) ss
        cut' []    []              = []
        cut' group []              = [reverse group]
        
partitions :: (Ord l, Show l) => [Directive r l] -> [(Int, [Stmt r Int])]
partitions p = map (fmap prune) $ map addContinue $ map (fmap (map resolveStmt)) parts'
  where parts = concatMap cut' $ groupLabels $ unifyLabels p 
        cut' (l, ss) = zip (l:repeat Nothing) (cut ss)
        
        (parts', labelMap) = runLabeller (mapM collectLabel parts) [0..]
          where collectLabel (l, ss) = do l' <- resolveLabel l                
                                          return (l', ss)
        
        lookup l = fromJust $ Map.lookup l labelMap
        
        resolveLabel Nothing = generate
        resolveLabel (Just l) = ensure l
        
        resolveStmt (Inc r) = Inc r
        resolveStmt (Dec r) = Dec r
        resolveStmt (Clr r) = Clr r
        resolveStmt (Mov r1 r2) = Mov r1 r2
        resolveStmt (Output r) = Output r
        resolveStmt (Input r) = Input r
        resolveStmt (Jmp l) = Jmp $ lookup l
        resolveStmt (Jz r l) = Jz r $ lookup l
        
        addContinue :: (Int, [Stmt r Int]) -> (Int, [Stmt r Int])
        addContinue (l, ss) = (l, ss ++ [Jmp (succ l)])
