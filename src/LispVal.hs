module LispVal (LispVal (..), readLispVals) where

import Text.ParserCombinators.Parsec

data LispVal = Number Integer

instance Show LispVal where show = showLispVal

showLispVal :: LispVal -> String
showLispVal (Number n) = show n

readLispVals :: String -> [LispVal]
readLispVals input = 
  case parse parseLispVals "lispval" input of
    Left err -> error $ "Error parsing LispVal: " ++ show err
    Right vals -> vals

parseLispVals :: Parser [LispVal]
parseLispVals = endBy parseLispVal (char '\n')

parseLispVal :: Parser LispVal
parseLispVal = parseNum

parseNum :: Parser LispVal
parseNum = do
  s <- many1 digit
  return $ Number (read s)
