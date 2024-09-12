--dobroLuhn :: Int -> Int
-- dobroLuhn x = if (dobro x > 9) then dobro x - 9 else dobro x
            --where dobro x = x*2;


--gera_lista n    | n>0 =  [n`mod`10] ++ (gera_lista (n`div`10))
--               | otherwise = []

gera_lista n    | n>0 =  [n`mod`10] : (gera_lista (n`div`10))
                | otherwise = []

-- a n = sum [x | x/]

-- luhn :: Int -> Int -> Int -> Int -> Bool
-- luhn n =  d(c(b(a n)))
