module Fraction (Fraction, (#)) where

data Fraction = Frac Integer Integer

-- class (Eq a, Show a) => Num a where
--    (+), (-), (*) :: a -> a -> a
--    negate :: a -> a
--    abs, signum :: a -> a
--    fromInteger :: Integer -> a

-- class (Num a) => Fractional a where
--    (/) :: a -> a -> a
--    recip :: a -> a
--    fromRational :: Rational -> a

num, denom :: Fraction -> Integer
num (Frac p q) = p
denom (Frac p q) = q

(#) :: Integer -> Integer -> Fraction
p#q
    | q==0 = error "#: division by zero"
    | q<0 = (-p) # (-q)
    | otherwise = Frac (p`div`d) (q`div`d)
    where d = mdc p q

mdc :: Integer -> Integer -> Integer
mdc a 0 = a
mdc a b = mdc b (a`mod`b)

instance Eq Fraction where
    (Frac p q) == (Frac r s) = p==r && q==s
instance Show Fraction where
    show (Frac p q) = show p ++ ('#': show q)

instance Num Fraction where
    (Frac p q) + (Frac r s) = (p*s+q*r) # (q*s) -- o operador (-) já vem implcíto (por causa do + e do negate creio eu)
    (Frac p q) * (Frac r s) = (p*r) # (q*s)
    negate (Frac p q) = Frac (-p) q
    fromInteger n = Frac n 1
    abs (Frac p q) 
        | p < 0 = Frac (-p) q   
        | otherwise = (Frac p q)
    signum (Frac p q)
        | p < 0 = -1
        | otherwise = 1

instance Ord Fraction where
    (Frac p q) <= (Frac r s) = p*s-q*r<=0 
