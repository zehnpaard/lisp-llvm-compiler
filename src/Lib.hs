module Lib
    ( someFunc
    ) where

import System.Environment

compile :: String -> String
compile = id

someFunc :: IO ()
someFunc = do
  args <- getArgs
  filename <- return $ head args
  source <- readFile filename
  writeFile (filename ++ ".ll") $ compile source
  putStrLn "Done"
