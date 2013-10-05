{-# LANGUAGE TupleSections #-}
module Language.MovDBz.FromBrainfuck where

import Language.Brainfuck.Syntax as BF
import Language.MovDBz.Syntax as MOVDBZ
import Data.Word

import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import Control.Arrow
import Data.Bifunctor
import Data.Maybe (fromMaybe)

import Data.Map (Map, (!))
import qualified Data.Map as Map

type CellAddr = Word16
type CellWidth = Word8

type Compile a = ReaderT CellAddr (State Int) a

data Reg = C0
         | CMaxData
         | CMaxAddr
         | Ptr
         | Tmp
         | Cell CellAddr
         deriving (Show, Eq, Ord)

-- It is very important to keep C0, CMaxData and CMaxAddr at the start
-- of the memory, since these will need to be initialized to non-0 values
layoutReg :: CellAddr -> Reg -> Int
layoutReg maxCell r = case r of
    C0        -> 0
    CMaxData  -> 1
    CMaxAddr  -> 2
    Ptr       -> 3
    Tmp       -> 4
    Cell cell -> 5 + fromIntegral cell

data ScanCont = Inc Label
              | Dec Label
              | Print Label
              | Branch Label Label
              deriving (Show, Eq, Ord)

data Label = Src Int
           | S SyntheticLabel
           deriving (Show, Eq, Ord)

data SyntheticLabel = Scan CellAddr ScanCont
                    | ScanFinished CellAddr ScanCont
                    | DecCell CellAddr Label
                    | DoIncCell CellAddr Label
                    | DoIncCellLoop CellAddr Label IncCellLabel
                    | DecUnderflowPtr Label
                    | UnderflowCell CellAddr Label
                    | DoIncPtr Label
                    | DoIncPtrLoop Label IncPtrLabel
                    | End
                    deriving (Show, Eq, Ord)

data IncPtrLabel = IncPtrDecCounter
                 | IncPtrDecPtr
                 | IncPtrUnderflowPtr
                 deriving (Show, Eq, Ord)

data IncCellLabel = IncCellDecCounter
                  | IncCellDecCell
                  | IncCellUnderflowCell
                  deriving (Show, Eq, Ord)

compileStmt :: BF.Stmt -> Compile (MOVDBZ.Program Reg Label)
compileStmt bf = do
    this <- Src <$> get
    case bf of
        While body -> do
            start <- Src <$> (modify succ *> get)
            prog <- compileStmts body
            loop <- Src <$> get
            next <- Src <$> (modify succ *> get)
            return $
              (this, jmp . S $ Scan 0 $ Branch next start) :
              (loop, jmp this) :
              prog
        _ -> do
            next <- Src <$> (modify succ *> get)
            return $ (:[]) . (this,) $ case bf of
                IncPtr -> jmp . S $ DoIncPtr next
                DecPtr -> MOVDBZ Ptr Ptr (S $ DecUnderflowPtr next) next
                IncData -> jmp . S $ Scan 0 $ Inc next
                DecData -> jmp . S $ Scan 0 $ Dec next
                Output -> jmp . S $ Scan 0 $ Print next
                Input -> error "Unsupported: Input"

compileStmts :: [BF.Stmt] -> Compile (MOVDBZ.Program Reg Label)
compileStmts = fmap concat . mapM compileStmt

-- "RTS" implementation
decUnderflowPtr :: Label -> (Label, MOVDBZ.Stmt Reg Label)
decUnderflowPtr next = (S $ DecUnderflowPtr next, MOVDBZ CMaxAddr Ptr (S End) next)

underflowCell :: CellAddr -> Label -> (Label, MOVDBZ.Stmt Reg Label)
underflowCell cell next = (S $ UnderflowCell cell next, MOVDBZ CMaxData (Cell cell) (S End) next)

incPtr :: Label -> [(Label, MOVDBZ.Stmt Reg Label)]
incPtr next = [ (enter,      MOVDBZ CMaxAddr Tmp decCounter decCounter)
              , (decCounter, MOVDBZ Tmp      Tmp next       decPtr    )
              , (decPtr,     MOVDBZ Ptr      Ptr underflow  decCounter)
              , (underflow,  MOVDBZ CMaxAddr Ptr decCounter decCounter)
              ]
  where
    enter = S $ DoIncPtr next
    decCounter = S . DoIncPtrLoop next $ IncPtrDecCounter
    decPtr =  S . DoIncPtrLoop next $ IncPtrDecPtr
    underflow = S . DoIncPtrLoop next $ IncPtrUnderflowPtr

decCell :: CellAddr -> Label -> [(Label, MOVDBZ.Stmt Reg Label)]
decCell cell next = [ (S $ DecCell cell next, MOVDBZ (Cell cell) (Cell cell) (S $ UnderflowCell cell next) next)
                    , underflowCell cell next
                    ]

incCell :: CellAddr -> Label -> [(Label, MOVDBZ.Stmt Reg Label)]
incCell cell next = [ (enter,      MOVDBZ CMaxData Tmp decCounter decCounter)
                    , (decCounter, MOVDBZ Tmp      Tmp next       decCell   )
                    , (decCell,    MOVDBZ r        r   underflow  decCounter)
                    , (underflow,  MOVDBZ CMaxData r   decCounter decCounter)
                    ]
  where
    r = Cell cell
    enter = S $ DoIncCell cell next
    decCounter = S . DoIncCellLoop cell next $ IncCellDecCounter
    decCell =  S . DoIncCellLoop cell next $ IncCellDecCell
    underflow = S . DoIncCellLoop cell next $ IncCellUnderflowCell

scan :: CellAddr -> ScanCont -> [(Label, MOVDBZ.Stmt Reg Label)]
scan maxCell cont = map (\i -> (S $ Scan i cont, step i)) [0..maxCell] ++
                    map (\i -> (S $ ScanFinished i cont, finish i)) [0..maxCell] ++
                    concatMap finalize [0..maxCell]
  where
    step i = MOVDBZ (if i == 0 then Ptr else Tmp) Tmp found nextStep
      where
        found = S $ ScanFinished i cont
        nextStep = S $ if i == maxCell then End else Scan (i+1) cont

    finish i = case cont of
        Print next -> PRINT (Cell i) next
        Dec next -> jmp . S $ DecCell i next
        Inc next -> jmp . S $ DoIncCell i next
        Branch ifZero els -> MOVDBZ (Cell i) Tmp ifZero els

    finalize i = case cont of
        Dec next -> decCell i next
        Inc next -> incCell i next
        _ -> []

jmp label = MOVDBZ C0 C0 label label


labelsOf (PRINT _ label) = [label]
labelsOf HALT = []
labelsOf (MOVDBZ _ _ label1 label2) = [label1, label2]

rtsFor :: CellAddr -> SyntheticLabel -> [(Label, MOVDBZ.Stmt Reg Label)]
rtsFor maxCell (Scan from cont) = if from == 0 then scan maxCell cont else error "Scan with non-zero starting value in non-RTS code"
rtsFor maxCell ScanFinished{} = error "ScanFinished in non-RTS code"
rtsFor maxCell (DecCell cell next) = decCell cell next
rtsFor maxCell (DoIncCell cell next) = incCell cell next
rtsFor maxCell (DecUnderflowPtr next) = [decUnderflowPtr next]
rtsFor maxCell UnderflowCell{} = error "UnderflowCell in non-RTS code"
rtsFor maxCell (DoIncPtr next) = incPtr next
rtsFor maxCell End = [(S End, HALT)]

doCompile :: CellAddr -> [BF.Stmt] -> MOVDBZ.Program Reg Label
doCompile maxCell bfs = flip evalState 0 $ flip runReaderT maxCell $ do
    prog <- compileStmts bfs
    label <- Src <$> get
    return $ prog ++ [(label, HALT)]

doRTS :: CellAddr -> MOVDBZ.Program Reg Label -> MOVDBZ.Program Reg Label
doRTS maxCell = concatMap (\lab -> case lab of S syn -> rtsFor maxCell syn; _ -> []) .
                concatMap (labelsOf . snd)

compileBF :: CellAddr -> [BF.Stmt] -> MOVDBZ.Program Int Int
compileBF maxCell bf = map (layoutLabel *** layout) prog'
  where
    prog = doCompile maxCell bf
    rts = (S End, HALT) : doRTS maxCell prog
    prog' = Map.toList . Map.fromList $ prog ++ rts

    labelMapping = Map.fromList $ zip (map fst prog') [0..]

    layoutLabel label = fromMaybe (error $ unwords ["no code generated for", show label]) $
                        Map.lookup label labelMapping

    layout :: MOVDBZ.Stmt Reg Label -> MOVDBZ.Stmt Int Int
    layout = bimap (layoutReg maxCell) layoutLabel

initialMemory :: CellAddr -> [Word16]
initialMemory maxCell = 0 : 256 : maxCell+1 : 0 : repeat 0

main = compileBF 10 testProg

testProg = [ IncData, IncData
           , While [ IncPtr
                   , IncData, IncData, IncData
                   , DecPtr
                   , DecData
                   ]
           , IncPtr
           , While [ Output, DecData ]
           ]
