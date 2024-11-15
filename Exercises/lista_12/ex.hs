------------------------ Questões 1 e 2 ------------------------

data Arv a = No a Int (Arv a) (Arv a)
            | Vazia
            deriving (Eq,Show)

altura :: Arv a -> Int
altura Vazia = 0
altura (No _ h esq dir) = h

-- funcao que colocara as anotacoes certa das alturas de uma arvore. Deve ser usada apenas na criacao da arvore.
set_altura :: Arv a -> Arv a
set_altura Vazia = Vazia
set_altura (No x _ esq dir) = No x (maximum [altura new_esq+1, altura new_dir+1]) new_esq new_dir
    where 
        profund (Vazia) = 0
        profund (No val _ esq dir) = maximum [1 + (profund esq), 1 + (profund dir)]
        new_esq = set_altura esq
        new_dir = set_altura dir

mais_esq :: Arv a -> a 
mais_esq (No x _ Vazia _) = x
mais_esq (No _ _ esq _) = mais_esq esq

mais_dir :: Arv a -> a
mais_dir (No x _ _ Vazia) = x
mais_dir (No _ _ _ dir) = mais_dir dir


desvio :: Arv a -> Int
desvio Vazia = 0
desvio (No _ _ esq dir) = altura esq - altura dir

pesquisaAVL :: Ord a => a -> Arv a -> Bool
pesquisaAVL x Vazia = False
pesquisaAVL x (No y _ esq dir)
    | x==y = True
    | x<y = pesquisaAVL x esq
    | x>y = pesquisaAVL x dir

rodar_dir :: Arv a -> Arv a
rodar_dir (No x _ (No y _ t1 t2) t3) = No y (maximum [altura t1+1, altura t2+2, altura t3+2]) t1 (No x (maximum [altura t2+1, altura t3+1]) t2 t3)
rodar_dir t = t               -- identidade nos outros casos

rodar_esq :: Arv a -> Arv a
rodar_esq (No x _ t1 (No y _ t2 t3)) = No y (maximum [altura t3+1, altura t1+2, altura t2+2]) (No x (maximum [altura t1+1, altura t2+1]) t1 t2) t3
rodar_esq t = t               -- identidade nos outros casos

corrige_dir :: Arv a -> Arv a
corrige_dir (No x h t1 t2)
    | desvio t1 == -1 = rodar_dir (No x (maximum [altura (rodar_esq t1)+1, altura t2+1]) (rodar_esq t1) t2)
    | otherwise       = rodar_dir (No x h t1 t2)
corrige_dir t = t             -- identidade noutros casos

corrige_esq :: Arv a -> Arv a
corrige_esq (No x h t1 t2)
    | desvio t2 == 1 = rodar_esq (No x (maximum [altura (rodar_dir t2)+1, altura t1+1]) t1 (rodar_dir t2))
    | otherwise      = rodar_esq (No x h t1 t2)
corrige_esq t = t             -- identidade noutros casos

rebalancear :: Arv a -> Arv a
rebalancear t
        | d== 2 = corrige_dir t
        | d== -2 = corrige_esq t
        | otherwise = t
    where d = desvio t

inserirAVL :: Ord a => a -> Arv a -> Arv a
inserirAVL x Vazia = No x 1 Vazia Vazia 
inserirAVL x (No y h esq dir)
    | x==y -- já ocorre
        = No y h esq dir
    | x<y -- inserir à esquerda
        = rebalancear (No y (maximum [altura (inserirAVL x esq)+1, altura dir+1]) (inserirAVL x esq) dir)
    | x>y -- inserir à direita
        = rebalancear (No y (maximum [altura esq+1, altura (inserirAVL x dir)+1]) esq (inserirAVL x dir))

remover :: Ord a => a -> Arv a -> Arv a
remover x Vazia = Vazia
remover x (No y _ Vazia dir) 
        | x==y = dir
remover x (No y _ esq Vazia)
        | x==y = esq
remover x (No y _ esq dir) 
        | x<y = rebalancear (No y (maximum [altura (remover x esq)+1, altura dir+1]) (remover x esq) dir)
        | x>y = rebalancear (No y (maximum [altura esq+1, altura (remover x dir)+1]) esq (remover x dir))
        | x==y = let z = mais_esq dir
            in rebalancear (No z (maximum [altura esq+1, altura (remover z dir)+1]) esq (remover z dir))

remover' :: Ord a => a -> Arv a -> Arv a
remover' x Vazia = Vazia
remover' x (No y _ Vazia dir) -- não ocorre
        | x==y = dir
remover' x (No y _ esq Vazia) -- um descendente
        | x==y = esq
remover' x (No y _ esq dir) -- dois descendentes
        | x<y = rebalancear (No y (maximum [altura (remover' x esq)+1, altura dir+1]) (remover' x esq) dir)
        | x>y = rebalancear (No y (maximum [altura esq+1, altura (remover' x dir)+1]) esq (remover' x dir))
        | x==y = let z = mais_dir esq
            in rebalancear (No z (maximum [altura (remover' z esq)+1, altura dir]+1) (remover' z esq) dir)

-- exemplo
arvore = set_altura (No 4 0 (No 2 0 (No 1 0 Vazia Vazia) (No 3 0 Vazia Vazia)) (No 6 0 (No 5 0 Vazia Vazia) (No 7 0 Vazia Vazia)))
arvore' = remover 4 arvore
arvore'' = inserirAVL 10 arvore'
