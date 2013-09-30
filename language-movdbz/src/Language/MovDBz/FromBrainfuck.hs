{-# LANGUAGE TupleSections #-}
module Language.MovDBz.FromBrainfuck where

import Language.Brainfuck.Syntax as BF
import Language.MovDBz.Syntax as MOVDBZ
import Data.Word

import Control.Monad.State
import Control.Applicative


type CellAddr = Word16
type CellWidth = Word8

data Reg = Ptr
         | Cell CellAddr
         | C0
         | CMaxData
         | CMaxAddr
         | Tmp
         deriving (Show, Eq)

data ScanCont = Inc Label
              | Dec Label
              | Print Label
              | Branch Label Label
              deriving (Show, Eq)

data Label = Src Int
           | Scan CellAddr ScanCont
           | Printer CellAddr Label
           | ScanFinished CellAddr ScanCont
           | DecCell CellAddr Label
           | DoIncCell CellAddr CellWidth Label
           | UnderflowPtr Label
           | UnderflowCell CellAddr Label
           | DoIncPtr CellAddr Label
           | End
           deriving (Show, Eq)

type Compile a = State Int a

compile1 :: CellAddr -> BF.Stmt -> Compile [(Label, MOVDBZ.Stmt Reg Label)]
compile1 maxCell bf = do
    this <- Src <$> get
    case bf of
        While body -> do
            start <- Src <$> (modify succ *> get)
            prog <- compile maxCell body
            loop <- Src <$> get
            next <- Src <$> (modify succ *> get)
            return $
              (this, jmp $ Scan maxCell $ Branch next start) :
              (loop, jmp this) :
              prog
        _ -> do
            next <- Src <$> (modify succ *> get)
            return $ (:[]) . (this,) $ case bf of
                IncPtr -> jmp $ DoIncPtr maxCell $ next
                DecPtr -> MOVDBZ Ptr Ptr (UnderflowPtr next) next
                IncData -> jmp $ Scan maxCell $ Inc next
                DecData -> jmp $ Scan maxCell $ Dec next
                Output -> jmp $ Scan maxCell $ Print next
                Input -> error "Unsupported: Input"
            
next :: Compile Label
next = Src <$> get

compile :: CellAddr -> [BF.Stmt] -> Compile [(Label, MOVDBZ.Stmt Reg Label)]
compile maxCell = fmap concat . mapM (compile1 maxCell)

-- "RTS"
underflowPtr :: Label -> (Label, MOVDBZ.Stmt Reg Label)
underflowPtr next = (UnderflowPtr next, MOVDBZ CMaxAddr Ptr End next)

underflowCell :: CellAddr -> Label -> (Label, MOVDBZ.Stmt Reg Label)
underflowCell cell next = (UnderflowCell cell next, MOVDBZ CMaxData (Cell cell) End next)

incPtr :: CellAddr -> Label -> [(Label, MOVDBZ.Stmt Reg Label)]
incPtr maxCell next = underflowPtr next :
                       map (\i -> underflowPtr (DoIncPtr (i+1) next)) [0..maxCell-1] ++
                       map (\i -> (DoIncPtr i next, step i)) [0..maxCell]
  where
    step i = MOVDBZ Ptr Ptr (UnderflowPtr nextStep) nextStep
      where
        nextStep = if i == maxCell then next else DoIncPtr (i + 1) next
    

decCell :: CellAddr -> Label -> [(Label, MOVDBZ.Stmt Reg Label)]
decCell cell next = [ (DecCell cell next,       MOVDBZ (Cell cell) (Cell cell) (UnderflowCell cell next) next) 
                    , underflowCell cell next
                    ]

incCell :: CellAddr -> Label -> [(Label, MOVDBZ.Stmt Reg Label)]
incCell cell next = underflowCell cell next :
                    map (\i -> underflowCell cell (DoIncCell cell (i+1) next)) [0..254] ++
                    map (\i -> (DoIncCell cell i next, step i)) [0..255]
  where
    step i = MOVDBZ (Cell cell) (Cell cell) (UnderflowCell cell nextStep) nextStep
      where
        nextStep = if i == maxBound then next else DoIncCell cell (i + 1) next

scan :: CellAddr -> ScanCont -> [(Label, MOVDBZ.Stmt Reg Label)]
scan maxCell cont = map (\i -> (Scan i cont, step i)) [0..maxCell] ++
                    map (\i -> (ScanFinished i cont, finish i)) [0..maxCell]
  where
    step i = MOVDBZ (if i == 0 then Ptr else Tmp) Tmp found nextStep
      where
        found = ScanFinished i cont
        nextStep = if i == maxCell then End else Scan (i+1) cont

    finish i = case cont of
        Print next -> PRINT (Cell i) next
        Dec next -> jmp $ DecCell i next
        Inc next -> jmp $ DoIncCell i 0 next
        Branch if0 els -> MOVDBZ (Cell i) Tmp if0 els

jmp label = MOVDBZ C0 C0 label label



test bfs = flip evalState 0 $ do
    prog <- compile 5 bfs
    label <- Src <$> get
    return $ prog ++ [(label, HALT)]

main = test [While [DecPtr]] -- , While [IncPtr, IncData]]
