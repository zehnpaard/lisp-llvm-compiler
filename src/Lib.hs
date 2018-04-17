module Lib
    ( someFunc
    ) where

import System.Environment
import Data.List.Split

import SSA
import SSAtoLlvm

someFunc :: IO ()
someFunc = do
  args <- getArgs
  filename <- return $ head args
  source <- readFile filename
  ssaString <- return $ concat $ map show $ readSSA source
  writeFile (filename ++ ".ssa") ssaString
  llvmString <- return $ ssasToLlvm $ readSSA $ ssaString
  writeFile (filename ++ ".ll") llvmString
  putStrLn "Done"
