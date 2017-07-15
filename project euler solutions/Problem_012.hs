module Main where

import Divisors (numDivisors)

trianNum n = ((n+1)*n) `div` 2

main = print $ trianNum.fst $ solve

-- brute force, optimized
solve = head $ dropWhile ((<500).snd) $ map tuple ([1::Int ..])
    where tuple :: Integral a => a -> (a, a)
          tuple n = (n, numDivisors $ trianNum n)
