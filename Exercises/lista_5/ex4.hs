-- selection sort

minimum_ :: Ord a => [a] -> a
minimum_ [x] = x
minimum_ (x:xs)     | x < minimum_ xs = x
                    | otherwise = minimum_ xs

delete :: Eq a=> a -> [a] -> [a]
delete n [] = []
delete n (x:xs)     | x == n = xs
                    | otherwise = [x] ++ (delete n xs)

ssort :: Ord a => [a] -> [a]
ssort [] = []
ssort lista = [minimum_ lista] ++ (ssort (delete (minimum_ lista) lista)) 