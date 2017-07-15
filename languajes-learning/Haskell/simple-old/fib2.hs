{-# LANGUAGE BangPatterns #-}
-- by helq Oct-2012

import Data.List (foldl')

-- Very fast (~0.137 sec, for 100000)
fib n = fib' 0 1 n
     where fib' a _  0  = a
           fib' a !b !n = fib' b (a+b) (n-1)

-- Very fast (a bit slow that fib) (~0.140 sec, for 100000)
fib2 n = fib' 0 1 n
     where fib' a _ 0 = a
           fib' a b n = let ab = a+b
                            n' = n-1
                        in seq ab $ seq n' (fib' b ab n')

-- Very fast, but a little slow (~0.5 sec, for 100000)
fib3 0 = 0
fib3 n = snd $ foldl' (\(a,b) _->(b,a+b)) (0,1) [1..n-1]

main = print $ fib2 100000

