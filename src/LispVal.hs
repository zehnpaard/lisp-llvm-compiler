module LispVal (LispVal (..), readLispVals) where

import Text.ParserCombinators.Parsec

data LispVal = Number Integer
             | Atom String
             | List [LispVal]

instance Show LispVal where show = showLispVal

showLispVal :: LispVal -> String
showLispVal (Number n) = show n
showLispVal (Atom s) = s

readLispVals :: String -> [LispVal]
readLispVals input = 
  case parse parseLispVals "lispval" input of
    Left err -> error $ "Error parsing LispVal: " ++ show err
    Right vals -> vals

parseLispVals :: Parser [LispVal]
parseLispVals = endBy parseLispVal (char '\n')

parseLispVal :: Parser LispVal
parseLispVal = parseNum 
           <|> parseAtom
           <|> parseList

parseNum :: Parser LispVal
parseNum = do
  s <- many1 digit
  return $ Number (read s)

parseList :: Parser LispVal
parseList = do
  char '('
  lvs <- sepBy parseLispVal (many1 space)
  char ')'
  return $ List lvs

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseAtom :: Parser LispVal
parseAtom = do
  first <- (letter <|> symbol)
  rest <- many (letter <|> digit <|> symbol)
  return $ Atom (first:rest)
