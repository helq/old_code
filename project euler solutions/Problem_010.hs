module Main where

import Primes (primes)

main = print $ sum $ takeWhile (< 2000000) primes
-- 142913828922
