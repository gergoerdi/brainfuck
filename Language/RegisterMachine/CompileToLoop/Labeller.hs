{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.RegisterMachine.CompileToLoop.Labeller (Labeller, runLabeller, generate, ensure) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

data Ord l => S l v = S { supply :: [v],
                         mapping :: Map l v }
                             
newtype Ord l => Labeller l v a = Labeller { unLabeller ::  State (S l v) a } deriving (Functor, Monad)

runLabeller :: Ord l => Labeller l v a -> [v] -> (a, Map l v)
runLabeller f vs = let (res, s) = runState (unLabeller f) $ S vs Map.empty
                   in (res, mapping s)

ensure :: Ord l => l -> Labeller l v v
ensure l = Labeller $ do
  existing <- gets $ Map.lookup l . mapping
  case existing of
    Just v  -> return v
    Nothing -> do S{ supply = v:vs, mapping = mapping } <- get
                  put S{ supply = vs, mapping = Map.insert l v mapping}
                  return v
      
generate :: Ord l => Labeller l v v               
generate = Labeller $ do
  s@S{ supply = v:vs} <- get
  put s{ supply = vs}
  return v
