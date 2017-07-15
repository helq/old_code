module Main where

import Data.List (foldl1', scanl1, transpose)

--matrix = transpose [[131, 673, 234, 103, 18], [201, 96, 342, 965, 150], [630, 803, 746, 422, 111], [537, 699, 497, 121, 956], [805, 732, 524, 37, 331]]

main = do
    file <- readFile "p82_matrix.txt"
    let matrix = transpose (map (\l->read $ '[':l++"]") . lines $ file) :: [[Int]]
    --print . transpose . scanl1 step $ matrix -- showing matrix with min paths
    print . foldl1' min . foldl1' step $ matrix

step xs@(x:_) ys = zipWith min suml sumr
    where suml = minl xs (x:ys)
          sumr = minr xs ys

minl _      [_]       = []
minl (x:xs) (y:y1:ys) = y' : minl xs (y':ys)
    where y' = min x y + y1

minr [x] [y] = [x+y]
minr (x:xs) (y:ys) = (min x z + y) : zs
    where zs@(z:_) = minr xs ys
