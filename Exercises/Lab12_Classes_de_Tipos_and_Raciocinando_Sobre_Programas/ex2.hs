data Arv a = Vazia | No a (Arv a) (Arv a)

listar :: Arv a -> [a]
listar t = listarAcc t []
--listar Vazia = []
--listar (No x esq dir) = listar esq ++ [x] ++ listar dir

listarAcc :: Arv a -> [a] -> [a] -- tal que listarAcc t xs = listar t ++ xs
listarAcc Vazia xs = xs
listarAcc (No x esq dir) xs = listarAcc esq (x:(listarAcc dir xs)) 
--listarAcc t xs = listar t ++ []
