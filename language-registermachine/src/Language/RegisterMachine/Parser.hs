module Language.RegisterMachine.Parser (parseRegisterMachine) where

import Language.RegisterMachine.Syntax
import Language.RegisterMachine.Syntax.Macros

import Control.Monad (liftM, guard)
import Text.ParserCombinators.Parsec hiding (label)
import Control.Applicative ((<$>), (<*>), (<*), (*>))

parseRegisterMachine = parseFromFile program

program = do
  optional newLine
  optional whiteSpace
  ms <- many macroDef
  ds <- directives
  eof
  return $ MacroProgram ms ds

macroDef = keyword "def" *> 
             (Macro <$> symbol <*> many symbol <* newLine <*> directives) 
           <* keyword "enddef"

stmt :: Parser PrimitiveStmt
stmt = inc <|> dec <|> clr <|> mov <|> input <|> output <|> jmp <|> jz <?> "Statement"
  where inc = keyword "inc" *> (Inc <$> register)
        dec = keyword "dec" *> (Dec <$> register)
        clr = keyword "clr" *> (Clr <$> register)          
        mov = keyword "mov" *> (Mov <$> register <*> register)        
        input = keyword "in" *> (Input <$> register)          
        output = keyword "out" *> (Output <$> register)          
        jmp = (try $ keyword "jmp") *> (Jmp <$> label)
        jz = keyword "jz" *> (Jz <$> register <*> label)
            
macroStmt = liftM PrimitiveStmt (try stmt) <|> add <|> macroCall
  where add = keyword "add" *> (Add <$> register <*> arg)
          
        macroCall = do
          macro <- symbol
          guard $ macro /= "enddef"
          MacroCall macro <$> many arg

directive = try label <|> liftM MacroStmt macroStmt
  where label = (MacroLabel <$> labelName) <* char ':'

directives = (try directive) `sepEndBy` newLine

register = symbol
label = lexeme labelName

labelName = genLabel <|> globalLabel
  where genLabel = char '_' *> (GenSym <$> symbolName)
        globalLabel = Global <$> symbolName
           
symbol = lexeme symbolName
symbolName = do
  c <- letter
  cs <- many $ (char '_' <|> letter <|> digit)
  return $ c:cs

keyword s = lexeme $ string s
          
arg = try int <|> sym
  where int = lexeme $ do
          s <- optionMaybe $ char '-'
          ds <- many1 digit
          let n = read ds :: Int
              n' = case s of
                Nothing -> n
                Just _ -> -n
          return $ Int n'
          
        sym = liftM Symbol symbol

inlineSpace = oneOf [' ', '\t'] >> return ()
newLine = optional commentLine >> skipMany1 (newline' <|> commentLine) >> whiteSpace
  where newline' = try (whiteSpace >> newline) >> return ()
              
whiteSpace :: Parser ()
whiteSpace = skipMany inlineSpace

commentLine :: Parser ()
commentLine = do
  _ <- try $ whiteSpace >> char ';'
  skipMany $ satisfy (/= '\n')
  return ()

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whiteSpace
  return x
