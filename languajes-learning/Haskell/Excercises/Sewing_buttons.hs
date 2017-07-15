-- Code by: helq
-- Date: 26-05-12 2012

comb n p
    | p > n     = 0
    | otherwise = (product [n-p+1..n]) `div` (product [1..p])

sew 0 _  = 1
sew n [] = 0
sew n (x:xs) = sum [ (comb n w)*(sew (n-w) xs) | w <- [0..min x n] ]

-- Example: sew 3 [2,4] ==> 7
-- XXX XXY XYX YXX XYY YXY YYX
