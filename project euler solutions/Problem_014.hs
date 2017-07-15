module Main where

import Data.List (foldl1')

main = print $ myMax $ map (\x-> (collatz x 0, x)) [1..1000000::Int]

collatz :: Int -> Int -> Int
collatz 1 t = 1+t
collatz n t
    | n `rem` 2 == 0 = let t'=t+1 in seq t' $ collatz (n `div` 2) t'
    | otherwise      = let t'=t+1 in seq t' $ collatz (3*n+1) t'

myMax :: [(Int, Int)] -> (Int, Int)
myMax xs = foldl1' (\(x,y) (x',y')-> if x>x' then (x,y) else (x',y') ) xs
