module SSAtoLlvm (ssasToLlvm) where

ssasToLlvm :: [[String]] -> String
ssasToLlvm ssas = start ++ (concat $ map ssaToLlvm  ssas) ++ end

ssaToLlvm :: [String] -> String
ssaToLlvm ["load", a, s] = "%" ++ a ++ " = load i32, i32* %" ++ s ++ "\n"
ssaToLlvm ["store", s, n] = "store i32 " ++ n ++ ", i32* %" ++ s ++ "\n"
ssaToLlvm ["alloc", s] = "%" ++ s ++ " = alloca i32" ++ "\n"
ssaToLlvm other = (concat $ map show other) ++ "\n"

start = ""
end = ""
