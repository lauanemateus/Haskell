import Data.List

------------------------------------ QUESTÃO 1 ------------------------------------
bits :: Int -> [[Bool]]
bits 0 = [[]]
bits n = [b:bs | bs<-bits (n-1), b<-[False,True]]

vars :: Prop -> [Char]
vars (Const _)    = []
vars (Var x)      = [x]
vars (Neg p)      = vars p
vars (Conj p q)   = vars p ++ vars q
vars (Disj p q)   = vars p ++ vars q
vars (Impl p q)   = vars p ++ vars q

atribs :: Prop -> [Atrib]
atribs p = map (zip vs) (bits (length vs))
    where vs = nub (vars p)

prop_falsa :: Prop -> [Assoc Char Bool]
prop_falsa p = [s | s<-atribs p, (valor s p) == False]

------------------------------------ QUESTÃO 2 ------------------------------------
data Prop =
            Const Bool -- constantes
            |Var Char -- variáveis
            |Neg Prop -- negação
            |Conj Prop Prop -- conjunção
            |Disj Prop Prop -- disjunção
            |Impl Prop Prop -- implicação
            |BiImpl Prop Prop -- bi-implicação
            deriving (Eq,Show)

type Assoc ch v = [(ch,v)]
find_ :: Eq ch => ch -> Assoc ch v -> v
find_ ch assocs = head [v | (ch',v)<-assocs, ch==ch']

type Atrib = Assoc Char Bool
valor :: Atrib -> Prop -> Bool
valor s (Const b)   = b
valor s (Var x)     = find_ x s
valor s (Neg p)     = not (valor s p)
valor s (Conj p q)  = valor s p && valor s q
valor s (Disj p q)  = valor s p || valor s q
valor s (Impl p q)  = not (valor s p) || valor s q
valor s (BiImpl p q) = valor s (Impl p q) && valor s (Impl q p)

-- usando a função valor
lista = [('a', True), ('b', False), ('c', True), ('d', False)]
prop1 = valor lista (Neg (Conj (Var 'a') (Var 'b')))
prop2 = valor lista (Disj (Const prop1) (Const True))
prop3 = valor lista (BiImpl (Const prop1) (Const prop2))
 
