divisores n = [x | x<-[1..n-1], n`mod`x==0]

perfeitos n = [x | x<-[1..n], x == (sum (divisores x))]