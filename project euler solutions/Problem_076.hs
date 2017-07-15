-- Slow: ~15 min
module Main where

sumWays :: Integral a => a -> [[a]]
sumWays n = f n n
    where
	f _ 0 = [[]]
	f m n = concat $ map (\x->map (x:) $ f x (n-x)) [q,q-1..1]
	    where q = min n m

main = putStrLn $ show $ (length $ sumWays (100::Int)) - 1

-- p n
--     | n == 0 = 1
--     | n <  0 = 0
--     | otherwise = sum $ map (\x->(sign x)*( p (n-x*(3*x+1)`div`2) + p (n-x*(3*x-1)`div`2) )) [1..n]
-- 	where sign x = if odd x then 1 else (negate 1)

-- main = putStrLn $ show $ p (20::Int)