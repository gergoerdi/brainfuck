module Language.Brainfuck.Parser (parseBrainFuck) where

import Language.Brainfuck.Syntax
import Text.ParserCombinators.Parsec
import Control.Applicative ((<*), (*>), (<$>))

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
      while   = symbol '[' *> (While <$> program) <* symbol ']'

parseBrainFuck = parseFromFile (program <* eof)
