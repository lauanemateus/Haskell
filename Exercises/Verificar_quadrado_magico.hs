-- verifica se uma matriz, na representação de lista das linhas, é um quadrado magico, ou seja, a matriz é nxn e todas suas linhas, colunas e diagonais tem mesma soma.

eh_quadrado_magico :: [[Integer]] -> Bool 
eh_quadrado_magico id = (and (map verifica_tam all_listas))  && (and (map verifica_sum all_listas)) 
    where
        n = sum (head id)
        verifica_sum xs = and [x==n| x<-(map sum xs)]
        verifica_tam xs = and [length x == length id | x<-xs]
        all_listas = [find_linhas id, find_colunas id, [find_diagonal1 id], [find_diagonal2 id]] 
        find_linhas xs = xs
        find_colunas [xs] = [[x] | x<-xs]
        find_colunas (xs:xss) = zipWith (:) xs (find_colunas xss)
        find_diagonal1 [xs] = xs
        find_diagonal1 (xs:xss) = (head xs) : find_diagonal1 [ tail xs' | xs'<-xss]
        find_diagonal2 xs = find_diagonal1 [reverse xs' | xs'<-xs]

exemplo1 = [[6, 7, 2], [1, 5, 9], [8, 3, 4]]
exemplo2 = [[6, 7, 2], [1, 5, 9], [2, 3, 4]]
exemplo3 = [[9, 6, 7, 2], [1, 5, 9], [8, 3, 4]]
