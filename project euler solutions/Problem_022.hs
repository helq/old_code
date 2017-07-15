module Main where

import Data.List (sort)
import Data.Char (ord)

main = do
    --file  <- readFile "018-triangle.txt"
    file  <- readFile "022-names.txt"
    let names = read ("["++file++"]") :: [String]

    print . sum . zipWith (\x y->x*score y) [1..] . sort $ names

score = sum . map (\x->ord x - ord 'A' + 1)
