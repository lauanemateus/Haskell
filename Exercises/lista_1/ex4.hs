divide_list :: [Int] -> ([Int], [Int])
divide_list [] = ([],[])
divide_list xs =    (take ((length xs) `div` 2) xs, 
                    drop ((length xs) `div` 2) xs)

-- teste lista = take ((length lista) `div` 2) lista
-- teste lista = take (div (length lista) 2) lista