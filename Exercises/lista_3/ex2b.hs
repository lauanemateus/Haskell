elemento :: Int -> Double
elemento k = ((-1)^k)/fromIntegral((k+1)^2)

f n = sqrt ( 12* sum [elemento x | x <-[0..n-1]])