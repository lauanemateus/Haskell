import Stack

verifica :: String -> Stack Char -> Bool
verifica [] pilha = (isEmpty pilha) 
verifica (x:xs) pilha 
    | x == '(' || x == '[' = verifica xs (push x pilha) 
    | (isEmpty pilha) == True = False
    | oposto (top pilha) x == False = False
    | otherwise = verifica xs (pop pilha)
    where 
        oposto x ')' = x == '('
        oposto x ']' = x == '['
        oposto _ _ = False 

parent :: String -> Bool
parent x = verifica x empty
  
