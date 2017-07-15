module Main where

sumWays n = f n n
    where
        f _ 0 = [[]]
        f m n = concat $ map (\x->map (x:) $ f x (n-x)) r
            where q = min n m
                  r = takeWhile (<=q) primes

primes = [2] ++ f [] [3,5..]
    where f xs (y:ys)
            | y `isDivCad` xs = f xs ys
            | otherwise       = y: f (xs++[y]) ys
                where isDivCad n xs = or $ map ((==0).(rem n)) xs

main = putStrLn $ show $ head $ dropWhile ((<5000).length.(sumWays)) [1..]

-- elem' n []      = False
-- elem' n (x:xs)
--     | n > x     = elem' n xs
--     | n == x    = True
--     | otherwise = False
-- 
-- isPrime n = elem' n primes

-- primes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]