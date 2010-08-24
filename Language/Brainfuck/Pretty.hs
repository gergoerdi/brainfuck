module Language.Brainfuck.Pretty (pPrintProgram) where

import Language.Brainfuck.Syntax
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import Data.List (intersperse)
    
pPrintProgram :: Program -> Doc
pPrintProgram = hcat . map pPrint

instance Pretty Stmt where
  pPrint IncPtr    = char '>'
  pPrint DecPtr    = char '<'
  pPrint IncData   = char '+'
  pPrint DecData   = char '-'
  pPrint Input     = char ','
  pPrint Output    = char '.'
  pPrint (While p) = brackets $ pPrintProgram p
