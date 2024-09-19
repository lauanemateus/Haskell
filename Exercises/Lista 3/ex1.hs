import Data.Char
import Data.String

----------------- Questão 1 -----------------
fat :: Int -> Int
fat n   | (n == 0 || n == 1) = 1       
        | otherwise = n * fat (n-1)

binom :: Int -> Int -> Int
binom n k = fat n `div` ((fat k)*fat(n-k))

pascal :: Int -> [[Int]]
pascal n = [ gera_linha x | x<-[0..n-1]]
    where
        gera_linha x = [ binom x y | y<-[0..x]]

----------------- Questão 2 -----------------
forte :: String -> Bool
forte senha = (length senha >= 8) && (or (map isLower senha)) && (or (map isUpper senha)) && (or (map isDigit senha))

----------------- Questão 3 -----------------  Cifra de Cézar
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c   | isLower c = int2let ((let2int c + n) `mod` 26)
            | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]


freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
    where 
        n = length xs

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x==x']

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2)/e | (o,e) <- zip os es]

rot :: Int -> [a] -> [a]
rot n xs = drop n xs ++ take n xs

crack :: String -> String
crack xs = to_maiusculas (encode (-fator) (to_minusculas xs))
    where
        to_minusculas string = [toLower x| x<-string]
        to_maiusculas string = [transforma (string !! i)  i | i<-[0..(length xs)-1]]
            where 
                transforma x i  | elem i (indicesMaiusculas xs) = toUpper x
                                | otherwise x
        fator = head (indices (minimum chitab) chitab)
        chitab = [chisqr (rot n tabela') tabela |
                n <- [0..25]]
        tabela' = freqs xs
        indices valor lista = [i | i<-[0..(length lista)-1], (lista !! i) == valor] 
        indicesMaiusculas string = [i | i<-[0..(length string)-1], isUpper (string !! i)] 

tabela :: [Float]
tabela = [ 14.6, 1.0, 3.9, 5.0, 12.6, 1.0, 1.3, 1.3, 6.2, 0.4, 0.02, 2.8, 4.7, 5.1, 10.7, 2.52, 1.2, 6.5, 7.8, 4.3, 4.6, 1.7, 0.01, 0.2, 0.01, 0.5 ]
