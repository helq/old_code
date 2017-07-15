module Main where
import Data.List
import Data.Char

fact9 :: Int -> Int
fact9 n = [1,1,2,6,24,120,720,5040,40320,362880] !! n

chainTerms n xs
    | n `elem` xs = xs
    | otherwise   = chainTerms (sum $ map (fact9.digitToInt) $ show n) (n:xs)

main = putStrLn $ show $ length $ filter ((>=60).length.(\x->chainTerms x [])) [1..10^6]