data Arv a = Vazia | No a (Arv a) (Arv a)

profund :: Arv a -> Int
profund (Vazia) = 0
profund (No val esq dir) = maximum [1 + (profund esq), 1 + (profund dir)]

teste = Vazia
teste2 = No 2 (No 3 Vazia Vazia) (No 4 Vazia Vazia)

tamanho :: Arv a -> Int
tamanho (Vazia) = 0
tamanho (No val esq dir) = 1 + tamanho esq + tamanho dir

eq_struct :: Arv a -> Arv b -> Bool
eq_struct Vazia Vazia = True
eq_struct (No a1 esq1 dir1) (No a2 esq2 dir2) = eq_struct esq1 esq2 && eq_struct dir1 dir2  

a1 = (No 3 (No 4 Vazia Vazia)(No 2 (No 7 Vazia Vazia)(No 1 Vazia Vazia)))
a2 = (No True (No True Vazia Vazia)(No False (No False Vazia Vazia)(No True Vazia Vazia)))
