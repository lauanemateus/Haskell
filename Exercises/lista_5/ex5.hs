-- gerando todas poibilidades de lista com valores contendo n bits

bits :: Int -> [[Bool]]
bits 1 = [[False], [True]]
bits n = [False:x | x<-(bits (n-1))] ++ [True:x | x<-(bits (n-1))]