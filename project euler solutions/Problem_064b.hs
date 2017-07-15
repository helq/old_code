-- glguy

module Main where
import Data.List

main = print $ length $ filter id $ map solve $ [2..9999] \\ (map (^2) [2..100])

solve n = even $ length $ cont n 0 1

cont :: Int -> Int -> Int -> [Int]
cont r n d = m : rest
 where
 m = truncate ((sqrt (fromIntegral r) + fromIntegral n ) / fromIntegral d)
 a = n - d * m
 rest = if d == 1 && n /= 0
         then []
         else cont r (-a) ((r - a ^ 2) `div` d)