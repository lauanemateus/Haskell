import Stack
import Set

------------------ Questão 1 ------------------

calc :: Stack Float -> String -> Stack Float
calc pilha num 
    | eh_operando num && size pilha < 2 = error "A expressão não é RPN"
    | eh_operando num =  push (operacao (top (pop pilha)) (top pilha) num) (pop (pop pilha))
    | otherwise = push (read num) pilha
    where 
        operacao x y c
            | c == "*" = x*y
            | c == "+" = x+y
            | c == "-" = x-y
            | c == "/" = x/y
        eh_operando c 
            | num == "*" || num == "/" || num == "+" || num == "-" = True
            | otherwise = False

calcular :: String -> Float
calcular exp = solve (words exp) empty
    where 
        lista_exp = words exp
        solve :: [String] -> Stack Float -> Float
        solve [] pilha 
            | (size pilha) > 1 = error "A expressão não é RPN"
            | otherwise = top pilha
        solve (x:xs) pilha = solve xs (calc pilha x)     

-- perguntar para o prof a letra (c)
-- RPN :: String -> IO ()
--    RPN exp = 

------------------ Questão 2 e 3 ------------------

-- fazer uns exemplos
-- (No 4 0 (No 2 0 (No 1 0 Vazia Vazia) (No 3 0 Vazia Vazia)) (No 6 0 (No 5 0 Vazia Vazia) (No 7 0 Vazia Vazia)))
