module Language.RegisterMachine.Parser (parseRegisterMachine) where

import Language.RegisterMachine.Syntax
import Language.RegisterMachine.Syntax.Macros

import Control.Monad (liftM, guard)
import Text.ParserCombinators.Parsec hiding (label)

parseRegisterMachine = parseFromFile program

program = do
  optional newLine
  optional whiteSpace
  ms <- many macroDef
  ds <- directives
  eof
  return $ MacroProgram ms ds

macroDef = do
  keyword "def"
  m <- symbol
  formals <- many symbol
  newLine
  ds <- directives
  keyword "enddef" >> newLine
  return $ Macro m formals ds

stmt :: Parser PrimitiveStmt
stmt = inc <|> dec <|> clr <|> input <|> output <|> jmp <|> jz <?> "Statement"
  where inc = do
          keyword "inc"
          r <- register
          return $ Inc r
          
        dec = do
          keyword "dec"
          r <- register
          return $ Dec r
          
        clr = do
          keyword "clr"
          r <- register
          return $ Clr r
          
        input = do
          keyword "in"
          r <- register
          return $ Input r
          
        output = do
          keyword "out"
          r <- register
          return $ Output r
          
        jmp = do
          try $ keyword "jmp"
          l <- label
          return $ Jmp l
                      
        jz = do
          keyword "jz"
          r <- register
          l <- label
          return $ Jz r l
            
macroStmt = liftM PrimitiveStmt (try stmt) <|> add <|> macroCall
  where add = do
          keyword "add"
          r <- register
          a <- arg
          return $ Add r a          
          
        macroCall = do
          macro <- symbol
          guard $ macro /= "enddef"
          args <- many arg
          return $ MacroCall macro args          

directive = try label <|> liftM MacroStmt macroStmt
  where label = do l <- labelName
                   char ':'
                   return $ MacroLabel l                   

directives = (try directive) `sepEndBy` newLine

register = symbol
label = lexeme labelName

labelName = genLabel <|> globalLabel
  where genLabel = do
          char '_'
          liftM GenSym symbolName
        globalLabel = liftM Global symbolName
           
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
  try $ whiteSpace >> char ';'
  skipMany $ satisfy (/= '\n')
  return ()

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  whiteSpace
  return x
