nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = [x] ++ nub [x'| x'<-xs, x' /= x]