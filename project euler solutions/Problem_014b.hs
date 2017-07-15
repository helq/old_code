module Main where

main = print $ myMax $ map (\x-> (collatz x, x)) [1..1000000::Int]

collatz :: Int -> Int
collatz 1 = 1
collatz n
    | n `rem` 2 == 0 = 1 + (collatz (n `div` 2))
    | otherwise      = 1 + (collatz (3*n+1))

myMax :: [(Int, Int)] -> (Int, Int)
myMax [(r, xs)]     = (r, xs)
myMax ((r, xs):xxs) = let (r',xs') = myMax xxs in
                          if r>r' then (r, xs) else (r', xs')
