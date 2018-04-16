module Lib
    ( someFunc
    ) where

import System.Environment
import Data.List.Split

import SSAtoLlvm

compile :: String -> String
compile = ssasToLlvm . map (filter (/="") . splitOn " ") . filter (/=""). splitOn "\n"

someFunc :: IO ()
someFunc = do
  args <- getArgs
  filename <- return $ head args
  source <- readFile filename
  writeFile (filename ++ ".ll") $ compile source
  putStrLn "Done"
