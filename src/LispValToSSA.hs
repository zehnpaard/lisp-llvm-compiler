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
