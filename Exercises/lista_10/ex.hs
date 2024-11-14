------------------------- Questão 1 ---------------------------

data Arv a = No a (Arv a) (Arv a)
            | Vazia
            deriving (Eq,Show)

mais_esq :: Arv a -> a
mais_esq (No x Vazia _) = x
mais_esq (No _ esq _) = mais_esq esq

mais_dir :: Arv a -> a
mais_dir (No x _ Vazia) = x
mais_dir (No _ _ dir) = mais_dir dir

------------------------- Questão 2 ---------------------------

remover :: Ord a => a -> Arv a -> Arv a
remover x Vazia = Vazia
remover x (No y Vazia dir) -- não ocorre
        | x==y = dir
remover x (No y esq Vazia) -- um descendente
        | x==y = esq
remover x (No y esq dir) -- dois descendentes
        | x<y = No y (remover x esq) dir
        | x>y = No y esq (remover x dir)
        | x==y = let z = mais_esq dir
            in No z esq (remover z dir)

remover' :: Ord a => a -> Arv a -> Arv a
remover' x Vazia = Vazia
remover' x (No y Vazia dir) -- não ocorre
        | x==y = dir
remover' x (No y esq Vazia) -- um descendente
        | x==y = esq
remover' x (No y esq dir) -- dois descendentes
        | x<y = No y (remover' x esq) dir
        | x>y = No y esq (remover' x dir)
        | x==y = let z = mais_dir esq
            in No z (remover z esq) dir

-- exemplo
arvore = No 4 (No 2 (No 1 Vazia Vazia) (No 3 Vazia Vazia)) (No 6 (No 5 Vazia Vazia) (No 7 Vazia Vazia))
