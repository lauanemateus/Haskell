elemento :: Integer -> Double
elemento i = ((-1)^i)/fromIntegral(2*i+1)

-- f :: Integer -> Double
f n = 4 * sum [elemento x | x<-[0..(n-1)]]