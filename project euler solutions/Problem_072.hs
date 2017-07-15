module Main where
import Control.Parallel (par, pseq)

main = putStrLn $ show $ 1 + 2*(a `par` (b `pseq` a + b))
    where
	a = f (10^6::Int) 4 1 3
	b = f (10^6::Int) 5 3 2
	f x n m o
	    | n <= x    = 1 + f x (n+m) m n + f x (n+o) n o
	    | otherwise = 0

-- See: Stern–Brocot tree

{-
          |
1         2         1
         /|\
        / | \
       /  |  \
      /   |   \
1    3    2    3    1
    /|\   |   /|\
1  4 3 5  2  5 3 4  1
-}

{- main = putStrLn $ show $ length $ filter id [y `isCoprime` x|x<-[1..1000000::Int],y<-[1..x-1]]
    where isCoprime n m = 1 == gcd n m-} --Very very slow