module SSA (SSA (..), readSSA) where

import Text.ParserCombinators.Parsec

data SSA = Alloc String
         | Store String String
         | Load String String

instance Show SSA where show = showSSA

showSSA :: SSA -> String
showSSA (Alloc s) = "alloc " ++ s ++ "\n"
showSSA (Store s n) = "store " ++ s ++ " " ++ n ++ "\n"
showSSA (Load a s) = "load " ++ a ++ " " ++ s ++ "\n"


readSSA :: String -> [SSA]
readSSA input = case parse parseSSAs "ssa" input of
  Left err  -> error $ "SSA parse failed: " ++ show err
  Right ssa -> ssa

parseSSAs :: Parser [SSA]
parseSSAs = endBy parseSSA (char '\n')

parseSSA :: Parser SSA
parseSSA = parseAlloc <|> parseStore <|> parseLoad

parseAlloc :: Parser SSA
parseAlloc = do 
  string "alloc"
  skipMany1 space
  s <- parseVar
  return $ Alloc s

parseStore :: Parser SSA
parseStore = do
  string "store"
  skipMany1 space
  s <- parseVar
  skipMany1 space
  n <- parseNum
  return $ Store s n

parseLoad :: Parser SSA
parseLoad = do
  string "load"
  skipMany1 space
  a <- parseVar
  skipMany1 space
  s <- parseVar
  return $ Load a s

parseVar :: Parser String
parseVar = do
  head <- letter
  rest <- many (letter <|> digit <|> (char '_'))
  return $ head:rest

parseNum :: Parser String
parseNum = many1 digit
