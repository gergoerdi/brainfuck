module Language.BrainFuck.Parser  where
-- module Language.BrainFuck.Parser (parseBrainFuck) where

import Language.BrainFuck.Syntax
    
import Text.ParserCombinators.Parsec
    
program = spaces >> stmt `sepEndBy` spaces
          
stmt = incptr <|> decptr <|> incdata <|> decdata <|> output <|> input <|> while
    where
      incptr  = char '>' >> return IncPtr
      decptr  = char '<' >> return DecPtr
      incdata = char '+' >> return IncData
      decdata = char '-' >> return DecData
      output  = char '.' >> return Output
      input   = char ',' >> return Input
      while   = do char '['
                   stmts <- try program
                   char ']'
                   return $ While stmts

parseBrainFuck = parseFromFile program'
    where program' = do p <- program
                        eof
                        return p
                 
