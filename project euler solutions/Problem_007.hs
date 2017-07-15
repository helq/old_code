module Main where

main = putStrLn $ show $ primes 10000

primes n = p [3,5,7,11,13] (n-5) [17,19..]
    where p xs n (y:ys)
            | n <= 0    = last xs
            | otherwise = if all (\x->rem y x /= 0) xs
                          then p (xs++[y]) (n-1) ys
                          else p xs n ys

