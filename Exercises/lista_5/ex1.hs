anyZero :: (Integer -> Integer) -> Integer -> Bool
anyZero f n = or [eh_zero x | x<-[0..n]] 
    where   eh_zero 0 = True 
            eh_zero _ = False