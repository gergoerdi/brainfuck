module Main where

import Language.Brainfuck.CompileToIA32
import Language.Brainfuck.Parser
import Language.IA32.Pretty
    
import Control.Monad.RWS
import Text.PrettyPrint.HughesPJClass (pPrint)
    
import System.Environment (getArgs)

main = do args <- getArgs
          case args of
            [filename] -> do parseRes <- parseBrainFuck filename
                             case parseRes of
                               Left err -> error (show err)
                               Right prog -> do let compiled = compile prog
                                                print $ pPrint $ compiled
            _ -> error "Usage: brainfuck filename.bf"
