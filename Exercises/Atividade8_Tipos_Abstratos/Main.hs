import Stack
import Set

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
calcular exp = solve (words exp) empty_stack
    where 
        lista_exp = words exp
        solve :: [String] -> Stack Float -> Float
        solve [] pilha 
            | (size pilha) > 1 = error "A expressão não é RPN"
            | otherwise = top pilha
        solve (x:xs) pilha = solve xs (calc pilha x)     

main = do
        putStr ("Escreva uma expressão em notação polonesa invertida (RPN)\n")
        expressao <- getLine
        putStr("O resultado é " ++ show (calcular expressao) ++ "\n")
        return ()
