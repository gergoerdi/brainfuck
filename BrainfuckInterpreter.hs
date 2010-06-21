{-# LANGUAGE DoRec #-}
module Main where

import Language.BrainFuck.Syntax
import Language.BrainFuck.Parser
import Language.BrainFuck.Interpreter
    
import System.Environment (getArgs)

parseAndRun filename = do parseRes <- parseBrainFuck filename
                          case parseRes of
                            Left err -> error (show err)
                            Right prog -> run prog >> return ()
                                   
main = do args <- getArgs
          case args of
            [filename] -> parseAndRun filename
            _ -> error "Usage: brainfuck filename.bf"
