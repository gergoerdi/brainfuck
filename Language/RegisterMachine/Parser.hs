module Language.RegisterMachine.Parser (parseRegisterMachine) where

import Language.RegisterMachine.Syntax
import Language.RegisterMachine.Syntax.Macros

import Control.Monad    
import Text.ParserCombinators.Parsec hiding (label)

inlineSpace = oneOf [' ', '\t'] >> return ()
newLine = optional commentLine >> skipMany1 (newline' <|> commentLine) >> whiteSpace
  where newline' = try (whiteSpace >> newline) >> return ()
              
whiteSpace :: Parser ()
whiteSpace = skipMany inlineSpace

commentLine :: Parser ()
commentLine = do
  try $ whiteSpace >> char ';'
  skipMany $ satisfy (/= '\n')
  return ()

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whiteSpace
  return x

reserved s = do
  lexeme $ string s

labelDef inMacro = do l <- label_
                      char ':'
                      whiteSpace <|> newLine
                      return l  
  where label_ | inMacro = try mlabel <|> label
               | otherwise = label
        
identifier = do
  c <- letter <|> digit
  cs <- many $ (char '_' <|> letter <|> digit)
  return $ c:cs

register = lexeme identifier

label' = identifier
mlabel' = char '_' >> label'

label = label' >>= return . GlobalLabel           
mlabel = do mlabel' >>= return . GenSymLabel
         
stmt inMacro = inc <|> dec <|> clr <|> input <|> jmp <|> jz
  where inc = do 
          reserved "inc"
          r <- register
          return $ Inc r
          
        dec = do
          reserved "dec"
          r <- register
          return $ Dec r
          
        clr = do
          reserved "clr"
          r <- register
          return $ Clr r
          
        jmp = do
          try $ reserved "jmp"
          l <- label_
          return $ Jmp l
          
        jz = do
          reserved "jz"
          r <- register
          l <- label_
          return $ Jz r l
          
        input = do
          reserved "print"
          r <- register
          return $ Output r
          
        label_ | inMacro = mlabel
               | otherwise = label
                  
macroName = lexeme identifier                             
                             
directive = try (stmt False >>= return . Stmt) <|> try (labelDef False >>= return . Label)
mdirective = try (stmt True >>= return . MStmt . RegStmt) <|> try (labelDef True >>= return . MLabel) <|> try (mcall >>= return . MStmt) <?> "Macro directive"
  where mcall = do macro <- macroName
                   guard $ macro /= "endprogram" && macro /= "enddef"
                   args <- many register
                   return $ MacroCall macro []
        
macrodef = do
  reserved "def"
  mname <- macroName
  formals <- many register
  newLine
  p <- mdirective `sepEndBy` newLine
  reserved "enddef"
  newLine
  return $ Macro mname formals p
  
          
program = do
  reserved "program"
  lexeme identifier
  newLine
  
  ms <- many macrodef
  p <- mdirective `sepEndBy` newLine
  
  reserved "endprogram"
  newLine  
  
  return $ MacroProgram ms p

parseRegisterMachine = parseFromFile program'
    where program' = do p <- program
                        eof
                        return p
                 
