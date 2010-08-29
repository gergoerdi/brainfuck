module Language.RegisterMachine.Syntax.Macros 
       (Symbol, RegisterSym, LabelSym(..), Arg(..), 
        PrimitiveStmt, MacroStmt(..), MacroDirective(..), 
        Macro(..), MacroProgram(..),
        processMacros)
       where

import Language.RegisterMachine.Syntax
import Data.Map (Map)
import qualified Data.Map as Map
import Language.RegisterMachine.CompileToLoop.Labeller
import Control.Monad.Reader
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

type Symbol = String

type RegisterSym = Symbol

data LabelSym = Global Symbol
              | GenSym Symbol
              deriving Show
                       
data Arg = Symbol Symbol
         | Int Int
         deriving Show

type PrimitiveStmt = Stmt RegisterSym LabelSym

data MacroStmt = PrimitiveStmt PrimitiveStmt
               | Add RegisterSym Arg
               | MacroCall Symbol [Arg]
               deriving Show
                 
data MacroDirective = MacroStmt MacroStmt                 
                    | MacroLabel LabelSym
                    deriving Show
                 
data Macro = Macro Symbol [Symbol] [MacroDirective] 
           deriving Show
                    
data MacroProgram = MacroProgram [Macro] [MacroDirective]
                  deriving Show

processMacros :: MacroProgram -> SourceProgram
processMacros (MacroProgram ms ds) = fst $ runLabeller (runReaderT (instantiateDirs ds) r) labels
  where r = R{ macros = macros,
               stack = [],
               actuals = Map.empty }
        macros = Map.fromList $ map toKV ms
          where toKV m@(Macro name _ _) = (name, m)
        labels = map (("_L"++) . show) [0..]

data R = R { macros :: Map Symbol Macro,
             stack :: [Symbol],
             actuals :: Map Symbol Arg} deriving Show
type Inst a = ReaderT R (Labeller Label Label) a

instantiateDirs = liftM concat . mapM instantiateDir

instantiateDir :: MacroDirective -> Inst [Directive Reg Label]
instantiateDir (MacroLabel l) = do
  l' <- instantiateLabel l
  return $ [Label l']
instantiateDir (MacroStmt s) = instantiateMacroStmt s

instantiateMacroStmt :: MacroStmt -> Inst [Directive Reg Label]
instantiateMacroStmt (PrimitiveStmt s) = do
  s' <- instantiateStmt s
  return [Stmt s']
instantiateMacroStmt (Add r arg) = do
  r' <- instantiateReg r
  arg' <- instantiateArg arg
  let n = case arg' of
        Int n -> n
  return $ replicate (abs n) $ Stmt $ (if n < 0 then Dec else Inc) r'
instantiateMacroStmt (MacroCall m args) = do
  args' <- mapM instantiateArg args
  instantiate m args'

instantiateArg (Symbol s) = do
  lookup <- asks $ Map.lookup s . actuals
  return $ case lookup of
    Nothing -> Symbol s
    Just v -> v
instantiateArg (Int n) = return $ Int n

instantiateLabel :: LabelSym -> Inst Label
instantiateLabel (Global l) = instantiateSymbol l
instantiateLabel (GenSym l) = lift $ ensure l

instantiateSymbol sym = do
  lookup <- asks $ Map.lookup sym . actuals
  case lookup of
    Nothing           -> return sym
    Just (Symbol sym) -> return sym
    Just (Int _)      -> error $ unwords ["argument is a number:", sym]

instantiateReg :: RegisterSym -> Inst Reg
instantiateReg = instantiateSymbol

instantiateStmt :: PrimitiveStmt -> Inst (Stmt Reg Label)
instantiateStmt (Inc r)  = return Inc `ap` instantiateReg r
instantiateStmt (Dec r)  = return Dec `ap` instantiateReg r
instantiateStmt (Clr r)  = return Clr `ap` instantiateReg r
instantiateStmt (Jmp l)  = return Jmp `ap` instantiateLabel l
instantiateStmt (Jz r l) = return Jz `ap` instantiateReg r `ap` instantiateLabel l
instantiateStmt (Input r) = return Input `ap` instantiateReg r
instantiateStmt (Output r) = return Output `ap` instantiateReg r

instantiate :: Symbol -> [Arg] -> Inst [Directive Reg Label]
instantiate m actuals = do
  stack <- asks stack
  when (elem m stack) $
    error $ unwords ["Recursive macro application:", intercalate ", " stack]
  Macro _ formals ds <- asks $ fromJust . Map.lookup m . macros
  let binds = zip formals actuals
  local (addBinds binds) $ instantiateDirs ds
  
  where addBinds binds r = r{ actuals = Map.fromList binds, stack = m:(stack r)}
