-- soma (x:xs) = x+soma(s)
-- f = soma [x^2 | x<-[1..100]]

f = sum [x^2 | x<-[1..100]]