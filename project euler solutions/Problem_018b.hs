module Main where

import Data.List (foldr1)

main = do
    --file  <- readFile "018-triangle.txt"
    file  <- readFile "067-triangle.txt"
    let triangle = ( map (map read.words) $ lines $ file ) :: [[Int]]

    print . foldr1 (\n m-> zipWith3 (\a b c->a + max b c) n m (tail m)) $ triangle
