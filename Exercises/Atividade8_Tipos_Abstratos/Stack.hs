module Stack (Stack, -- exportar o tipo
push, pop, top, -- exportar as operac¸oes ˜
empty_stack, isEmpty, size) where

data Stack a = Stk [a] -- implementação usando listas 
    deriving(Show)

push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)

pop :: Stack a -> Stack a
pop (Stk (_:xs)) = Stk xs
pop _ = error "Stack.pop: empty stack"

top :: Stack a -> a
top (Stk (x:_)) = x
top _ = error "Stack.top: empty stack"

empty_stack :: Stack a
empty_stack = Stk []

isEmpty :: Stack a -> Bool
isEmpty (Stk [])= True
isEmpty (Stk _) = False

size :: Stack a -> Int 
size (Stk []) = 0
size (Stk (_:xs)) = 1 + size (Stk xs)
