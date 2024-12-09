-------------------------------- Questão 1 --------------------------------

data Arv a = Vazia | No a (Arv a) (Arv a)

listar :: Arv a -> [a]
listar t = listarAcc t []
--listar Vazia = []
--listar (No x esq dir) = listar esq ++ [x] ++ listar dir

listarAcc :: Arv a -> [a] -> [a] -- tal que listarAcc t xs = listar t ++ xs
listarAcc Vazia xs = xs
listarAcc (No x esq dir) xs = listarAcc esq (x:(listarAcc dir xs)) 

-------------------------------- Questão 2 --------------------------------

soma :: Arv Integer -> Integer
soma Vazia = 0
soma (No x esq dir) = x + (soma esq) + (soma dir)

foldArv :: (a -> b -> b -> b) -> b -> Arv a -> b -- foldArv f v t
foldArv f v Vazia = v
foldArv f v (No x esq dir) = f x (foldArv f v esq) (foldArv f v dir)

soma3 x y z = x + y + z

{-
    2-b) Provar que (soma t = foldArv soma3 0 t)
    
    Caso base: quando t = Vazia, temos (foldArv soma3 0 Vazia = 0 = soma Vazia)
    Hipótese de indução: Suponha que (soma esq = foldArv soma3 esq) e (soma dir = foldArv soma 3 dir)

    Para 
        foldArv soma3 (No x esq dir)    = soma3 x (foldArv soma3 v esq) (foldArv soma3 v dir) -- função foldArv
                                        = soma3 x (soma esq) (soma dir) -- hipótese de indução
                                        = x + (soma esq) + (soma dir) -- aplicando função soma3
                                        = soma (No x esq dir) -- definição de soma
-}

-------------------------------- Questão 3 --------------------------------

{-

    3-a) Provar que (map f (xs++ys) = map f xs ++ map f ys)

    Casos bases:
        xs = [] => (map f (xs++ys) = map f ([]++ys) = map f ys = [] ++ map f ys = map f [] ++ map f ys) 
        ys = [] => (map f (xs++ys) = map f (xs++[]) = map f xs = map f xs ++ [] = map f xs ++ map f [])  
    Hipótese de indução: Suponha que map f (xs++ys) = map f xs ++ map f ys

    Para map f ((x:xs)++ys)     = map f (x:(xs++ys)) -- associatividade concatenação/cons
                                = [f x] ++ map f (xs++ys) -- definição map
                                = [f x] ++ (map f xs ++ map f ys) -- hipótese de indução
                                = ([f x] ++ map f xs) ++ map f ys -- associatividade concatenação/cons
                                = map f (x:xs) ++ map f ys -- definição map  

    3-b) Provar que (map f (reverse xs) = reverse (map f xs))

    Caso base: xs = [] => map f (reverse xs)    = map f (reverse [])
                                                = map f []
                                                = []

    Hipótese de indução: Suponha que (map f (reverse xs) = reverse (map f xs))

    Para (map f (reverse (x:xs))    = map f (reverse xs ++ [x]) -- distributividade reverse/++
                                    = map f (reverse xs) ++ map f [x] -- propriedade item 3-a)
                                    = reverse (map f xs) ++ map f [x] -- hipótese de indução
                                    = reverse ((map f [x]) ++ (map f xs) ) -- distributividade reverse/++
                                    = reverse (map f ([x]++xs)) -- propriedade item 3-a)
                                    = reverse map f (x:xs) -- definição concatenação/cons

-}

-- exemplo de árvores 
arv1 = No 7 (No 5 (No 3 (No 1 Vazia (No 2 Vazia Vazia)) (No 4 Vazia Vazia)) (No 6 Vazia Vazia)) Vazia
arv2 = No 7 (No 2 (No 1 Vazia Vazia) (No 4 Vazia Vazia)) (No 9 (No 8 Vazia Vazia) (No 10 Vazia Vazia))
