module Set (Set, insert, empty_set, member, union, intersect, difference) where

data Set a = No a (Set a) (Set a)
            | Vazia
            deriving (Eq,Show)
            
empty_set :: Set a
empty_set = Vazia

insert :: Ord a => a -> Set a -> Set a
insert x Vazia = No x Vazia Vazia
insert x (No y esq dir) 
    | x==y = No y esq dir -- já ocorre
    | x<y = No y (insert x esq) dir -- insere à esquerda
    | x>y = No y esq (insert x dir ) -- insere à direita

member :: Ord a => a -> Set a -> Bool
member x Vazia = False -- não ocorre
member x (No y esq dir)
    | x==y = True -- encontrou
    | x<y = member x esq -- procura à esquerda
    | x>y = member x dir -- procura à direita

union :: Ord a => Set a -> Set a -> Set a
union conj1 Vazia = conj1
union Vazia conj2 = conj2
union conj1 (No x esq dir) = union (insert x conj1) (union esq dir)

intersect :: Ord a => Set a -> Set a -> Set a
intersect conj1 conj2 = intersectAcc conj1 conj2 empty_set
    where 
        intersectAcc conj1 Vazia resposta = resposta
        intersectAcc Vazia conj2 resposta = resposta
        intersectAcc conj1 (No x esq dir) resposta 
            | member x conj1 = intersectAcc conj1 (union esq dir) (insert x resposta)
            | otherwise = intersectAcc conj1 (union esq dir) resposta

difference :: Ord a => Set a -> Set a -> Set a
difference conj1 conj2 = differenceAcc conj1 conj2 empty_set
    where 
        differenceAcc conj1 Vazia resposta = union resposta conj1
        differenceAcc Vazia conj2 resposta = resposta
        differenceAcc (No x esq dir) conj2 resposta 
            | member x conj2 = differenceAcc (union esq dir) conj2 resposta
            | otherwise = differenceAcc (union esq dir) conj2 (insert x resposta)

-- exemplo
conjunto1 = No 7 (No 5 (No 3 (No 1 Vazia (No 2 Vazia Vazia)) (No 4 Vazia Vazia)) (No 6 Vazia Vazia)) Vazia
conjunto2 = No 7 (No 2 (No 1 Vazia Vazia) (No 4 Vazia Vazia)) (No 9 (No 8 Vazia Vazia) (No 10 Vazia Vazia))
