module Main where

import Data.List (foldl1, scanl1)

--(fstrow:matrix) = [[131, 673, 234, 103, 18 ], [201, 96,  342, 965, 150], [630, 803, 746, 422, 111], [537, 699, 497, 121, 956], [805, 732, 524, 37,  331]]

main = do
    file <- readFile "081-matrix.txt"
    let (fstrow:matrix) = (map (\l->read $ '[':l++"]") . lines $ file) :: [[Int]]
    --print . scanl1 step $ (scanl1 (+) fstrow) : matrix -- showing matrix with min paths
    print . last . foldl1 step $ (scanl1 (+) fstrow) : matrix

step xs@(x:_) ys = minl xs (x:ys)

minl _      [_]       = []
minl (x:xs) (y:y1:ys) = y' : minl xs (y':ys)
    where y' = (min x y + y1)
