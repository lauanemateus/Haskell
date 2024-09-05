half_of_perimeter (a, b, c) = (a+b+c)/2
triangule_area a b c = sqrt (s*(s-a)*(s-b)*(s-c))
                        where s = half_of_perimeter (a, b, c)