module Set (Set, empty, insert, member) where

data Set a = No a (Set a) (Set a) | Vazia
            deriving (Eq,Show)

empty :: Set a
empty = Vazia

insert :: Ord a => a -> Set a -> Set a
insert x Vazia = No x Vazia Vazia
insert x (No y esq dir)
    | x == y -- já ocorre
        = (No y esq dir)
    | x<y = (No y (insert x esq) dir)
    | x>y = (No y esq (insert x dir)) 

member :: Ord a => a -> Set a -> Bool
member x Vazia = False
member x (No y esq dir)
    | x == y = True
    | x<y = (member x esq)
    | x>y = (member x dir)

