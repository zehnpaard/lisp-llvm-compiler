module LispValToSSA (lispValsToSSAs) where

import LispVal
import SSA

lispValsToSSAs :: [LispVal] -> [SSA]
lispValsToSSAs = concat . reverse . zipWith lispValToSSA [[x] | x <- [1..]] . reverse

lispValToSSA :: [Integer] -> LispVal -> [SSA]

lispValToSSA is (Number n) = [Alloc s, Store s (show n), Load a s]
  where t = concat $ tail $ concat $ [["_", show i] | i <- is]
        s = 's':t
        a = 'a':t

lispValToSSA is (List [Atom "+", a, b]) = 
  lispValToSSA (is ++ [2]) a ++ lispValToSSA (is ++ [1]) b ++ [Add a1 a2 a3]
  where t = concat $ tail $ concat $ [["_", show i] | i <- is]
        a1 = 'a':t
        a2 = 'a':t ++ "_2"
        a3 = 'a':t ++ "_1"
