module Language.Brainfuck.Parser (parseBrainFuck) where

import Language.Brainfuck.Syntax
    
import Text.ParserCombinators.Parsec

symbol c = spaces >> char c >> spaces    
    
program = many stmt
          
stmt = incPtr <|> decPtr <|> incData <|> decData <|> output <|> input <|> while
    where
      incPtr  = symbol '>' >> return IncPtr
      decPtr  = symbol '<' >> return DecPtr
      incData = symbol '+' >> return IncData
      decData = symbol '-' >> return DecData
      output  = symbol '.' >> return Output
      input   = symbol ',' >> return Input
      while   = do symbol '['
                   stmts <- program
                   symbol ']'
                   return $ While stmts

parseBrainFuck = parseFromFile program'
    where program' = do p <- program
                        eof
                        return p
                 
