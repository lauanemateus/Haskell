-------------------------Questão 1--------------------------------

intersperse :: a -> [a] -> [a]
intersperse c [] = []
intersperse c (x:xs) = [x] ++ [c] ++ (intersperse c xs)

-------------------------Questão 2--------------------------------

permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations lista = conjunto (gera_permutations lista)
    where 
        delete x' (x:xs)    | x == x' = xs
                            | xs == [] = [x]
                            | otherwise = x:(delete x' xs)
        gera_permutations lista = [x:xs | x<-lista, xs<-permutations(delete x lista)]
        conjunto [] = []
        conjunto (x:xs) = x:[x'|x'<-xs, x'/=x] 

-------------------------Questão 3--------------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge [] x = x
merge x [] = x
merge (x:xs) (x':xs')   | x<x' = x:(merge xs (x':xs'))
                        | otherwise = x':(merge (x:xs) xs')

msort :: Ord a=> [a]-> [a]
msort [] = []
msort [x] = [x]
msort lista = merge (msort (take ((length lista)`div`2) lista)) (msort (drop ((length lista)`div`2) lista))
