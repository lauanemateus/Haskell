-- inerction sort

insert :: Ord a => a -> [a] -> [a]
insert  n [] = [n]
insert  n (x:xs)    | x<n = [x] ++ insert n xs
                    | otherwise = [n] ++ [x] ++ xs

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)