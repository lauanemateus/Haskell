import Stack

makeStack :: [Char] -> Stack Char
makeStack [] = empty
makeStack (x:xs)  
    | x == ')'  = push x (makeStack xs)
    | x == '(' && isEmpty (makeStack xs) = push '*' empty
    | x == '(' && top (makeStack xs) == ')' = pop (makeStack xs)

parent :: String -> Bool
parent x = isEmpty (makeStack x )
  
