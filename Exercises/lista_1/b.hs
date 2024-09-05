is_triangulo a b c = a < b+c &&  b < a+c &&  c < a+b 

-- another way of do it
is_triangulo2 (a, b, c) = a < b+c &&  b < a+c &&  c < a+b 
