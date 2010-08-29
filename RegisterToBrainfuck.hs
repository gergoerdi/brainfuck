{-# LANGUAGE DoRec, GeneralizedNewtypeDeriving #-}
module Main where

import Language.RegisterMachine.Syntax.Macros
import Language.RegisterMachine.Parser
import Language.RegisterMachine.CompileToLoop 
import Language.RegisterMachine.CompileToLoop.Partitions
import Language.Loop.CompileToBrainfuck
import Language.Brainfuck.Pretty

import IPPrint

import System (getProgName)
import System.Environment (getArgs)

main = do args <- getArgs
          case args of
            [filename] -> do parseRes <- parseRegisterMachine filename                             
                             case parseRes of
                               Left err -> error (show err)
                               Right p -> do let p' = processMacros p
                                                 loop = toLoop p'
                                                 bf = toBrainfuck loop
                                             print $ pPrintProgram bf
            _ -> do self <- getProgName
                    error $ unwords ["Usage:", self, "filename.bf"]
