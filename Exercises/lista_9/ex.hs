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
prop_falsa p = [s | s<-atribs p, (valor_prop s p) == False]

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
valor_prop :: Atrib -> Prop -> Bool
valor_prop s (Const b)   = b
valor_prop s (Var x)     = find_ x s
valor_prop s (Neg p)     = not (valor_prop s p)
valor_prop s (Conj p q)  = valor_prop s p && valor_prop s q
valor_prop s (Disj p q)  = valor_prop s p || valor_prop s q
valor_prop s (Impl p q)  = not (valor_prop s p) || valor_prop s q
valor_prop s (BiImpl p q) = valor_prop s (Impl p q) && valor_prop s (Impl q p)

-- usando a função valor_prop
lista = [('a', True), ('b', False), ('c', True), ('d', False)]
prop1 = valor_prop lista (Neg (Conj (Var 'a') (Var 'b')))
prop2 = valor_prop lista (Disj (Const prop1) (Const True))
prop3 = valor_prop lista (BiImpl (Const prop1) (Const prop2))
 
------------------------------------ QUESTÃO 3 ------------------------------------
data Expr = Val Int
        |   Soma Expr Expr
        |   Mult Expr Expr

type Cont = [Op]
data Op = SOMA Expr | MULT Expr | VAL_SOMA Int | VAL_MULT Int

aval :: Expr -> Cont -> Int
aval (Val n) c = exec c n
aval (Soma x y) c = aval x (SOMA y : c)
aval (Mult x y) c = aval x (MULT y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (SOMA y : c) n = aval y (VAL_SOMA n : c)
exec (VAL_SOMA n : c) m = exec c (n+m)
exec (MULT y : c) n = aval y (VAL_MULT n : c)
exec (VAL_MULT n : c) m = exec c (n*m)

valor :: Expr -> Int
valor e = aval e []

-- testando
expr1 = valor (Mult (Soma (Val 2) (Val 5)) (Soma (Val 7) (Val 1))) -- (2+5)*(7+1)
expr2 = valor (Soma (Val 10) (Mult (Val 5) (Soma (Val 6) (Val 7)))) -- (10 + (5 * (6 + 7)))
expr3 = valor (Soma (Mult (Val 8) (Soma (Val 3) (Mult (Val 4) (Val 2)))) (Soma (Val 10) (Mult (Val 5) (Val 5)))) -- (8 * (3 + (4 * 2))) + (10 + (5 * 5))
