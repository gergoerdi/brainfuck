module Main where

import Language.Brainfuck.Parser
-- import Language.Brainfuck.Interpreter.IO
import Language.Brainfuck.Interpreter
    
import System.Environment (getProgName, getArgs)

parseAndRun filename = do parseRes <- parseBrainFuck filename
                          case parseRes of
                            Left err -> error (show err)
                            Right prog -> run prog >> return ()
                                   
main = do args <- getArgs
          case args of
            [filename] -> parseAndRun filename
            _ -> do self <- getProgName
                    error $ unwords ["Usage:", self, "filename.bf"]
