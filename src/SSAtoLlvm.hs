module SSAtoLlvm (ssasToLlvm) where

import SSA

ssasToLlvm :: [SSA] -> String
ssasToLlvm ssas = start ++ (concat $ map ssaToLlvm  ssas) ++ end

ssaToLlvm :: SSA -> String
ssaToLlvm (Alloc s) = "%" ++ s ++ " = alloca i32" ++ "\n"
ssaToLlvm (Store s n) = "store i32 " ++ n ++ ", i32* %" ++ s ++ "\n"
ssaToLlvm (Load a s) = "%" ++ a ++ " = load i32, i32* %" ++ s ++ "\n"
ssaToLlvm (Add a1 a2 a3) = "%" ++ a1 ++ " = add i32 %" ++ a2 ++ ", %" ++ a3 ++ "\n"

start = " @.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\"\n\ndefine i32 @lispfn(){\n"
end = "ret i32 %a1\n}\n\ndefine i32 @main() {\n%call = call i32 @lispfn()\n%call1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str, i32 0, i32 0), i32 %call)\nret i32 0\n}\n\ndeclare i32 @printf(i8*, ...)\n"
