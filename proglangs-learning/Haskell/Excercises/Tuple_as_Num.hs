-- by helq

instance Eq a => Eq (a,a) where
    (a, b) == (c, d) = a==c && b==d

instance (Num a, Eq a, Ord a) => Num (a,a) where
    (a,b) + (c,d) = (a+c, b+d)
    (a,b) * (c,d) = (a*d+b*c, a*c+b*d)
    (a,b) - (c,d) = (a-c, b-d)
    negate (a,b)  = (negate a, negate b)
    abs    (a,b)  = (abs a, abs b)
    signum (a,b)
        | a==b      = 0
        | a<b       = -1
        | otherwise = 1
    fromInteger a = (fromInteger a, fromInteger a)
