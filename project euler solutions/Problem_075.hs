module Main where
-- execute: ghc Problem_075 && ./Problem_075 > 075-total.txt && ./Problem_075.py

import qualified Data.Set as Set

-- filterMul :: Integral b => [b] -> [b]
-- filterMul xs = filter' [] xs
--     where
--       filter' ys  []      = ys
--       filter' ys (x:xs)
--         | pred' x         = filter' (ys++[x]) xs
--         | otherwise       = filter' ys xs
--           where pred' x   = all ((/=0).(x`rem`)) (takeWhile (<x) ys)

main = putStrLn $ show $ Set.toList $ Set.fromList [trian x y | x<-[1..866], y<-[x+1,x+3..867], not $ any ((==0).(y`rem`)) (factorPrimes900 x)]

trian n m = 2*m^2+2*m*n

primes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449]

factorPrimes900 x = filter ((==0).(x`rem`)) primes
