import Stack

constroi 0 = empty
constroi n = push n (constroi (n-1))

teste = push 10 empty
