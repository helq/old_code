module Main where

-- from: http://zenzike.com/posts/euler/2010-07-08-euler-2
fibs   :: [Integer]
fibs   = 0 : fibs'
fibs'  = 1 : fibs''
fibs'' = zipWith (+) fibs fibs'

-- from me (helq)
fibs2 :: [Integer]
fibs2 = f 0 1
    where f a b = a : f b (a+b)

main = print $ zip [1..] fibs
