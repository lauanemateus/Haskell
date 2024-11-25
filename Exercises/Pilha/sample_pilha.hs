import Stack

constroi 0 = empty
constroi n = push n (constroi (n-1))