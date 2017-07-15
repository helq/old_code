-- by helq

main = print $ comb' 200 [100,200,50,20,10,5,2,1]

comb' :: Integral a => a -> [a] -> a
comb' 0 _  = 1
comb' _ [] = 0
comb' n (x:xs) | n < 0     = 0
               | otherwise = comb' (n-x) (x:xs) + comb' n xs
