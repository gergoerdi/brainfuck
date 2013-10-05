module Language.MovDBz.FromBrainfuck.Example (main) where

import Language.MovDBz.FromBrainfuck
import Language.MovDBz.Interpreter
import Language.Brainfuck.Syntax as BF
import Data.Word

runBFviaMovDBz :: Word16 -> [BF.Stmt] -> IO ()
runBFviaMovDBz maxCell prog = run (length prog') (fromIntegral $ 5 + maxCell) prog' (initialMemory maxCell)
  where
    prog' = layout maxCell . compileBF maxCell $ prog

main :: IO ()
main = runBFviaMovDBz 2 $
          [ IncData, IncData
          , While [ IncPtr
                  , IncData, IncData, IncData
                  , DecPtr
                  , DecData
                  ]
          , IncPtr
          , While [ Output, DecData ]
          ]
