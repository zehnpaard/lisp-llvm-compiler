module Lib
    ( someFunc
    ) where

import System.Environment
import Data.List.Split

import LispVal
import LispValToSSA
import SSA
import SSAtoLlvm

someFunc :: IO ()
someFunc = do
  args <- getArgs
  filename <- return $ head args
  source <- readFile filename
  lispVals <- return $ readLispVals source
  ssas <- return $ lispValsToSSAs lispVals
  ssaString <- return $ concat $ map show $ ssas 
  writeFile (filename ++ ".ssa") ssaString
  llvmString <- return $ ssasToLlvm $ readSSA $ ssaString
  writeFile (filename ++ ".ll") llvmString
  putStrLn "Done"
