{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.RegisterMachine.CompileToLoop.Labeller (Labeller, runLabeller, generate, ensure, scope) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

data S l v = S { supply :: [v],
                 mapping :: Map l v }
                             
newtype Labeller l v a = Labeller { unLabeller ::  State (S l v) a } deriving (Functor, Monad)

runLabeller :: Labeller l v a -> [v] -> (a, Map l v)
runLabeller f vs = let (res, s) = runState (unLabeller f) $ S vs Map.empty
                   in (res, mapping s)

ensure :: Ord l => l -> Labeller l v v
ensure l = Labeller $ do
  existing <- gets $ Map.lookup l . mapping
  case existing of
    Just v  -> return v
    Nothing -> do v <- generate'
                  modify $ \s -> s{ mapping = Map.insert l v $ mapping s}
                  return v
  where generate' = unLabeller generate
      
generate :: Labeller l v v               
generate = Labeller $ do
  s@S{ supply = v:vs} <- get
  put s{ supply = vs}
  return v

scope :: Labeller l v a -> Labeller l v a
scope f = Labeller $ do
  s <- get
  let s' = S (supply s) Map.empty
  put s'
  res <- unLabeller f
  s'' <- get
  put $ s{supply = supply s''}
  return res
