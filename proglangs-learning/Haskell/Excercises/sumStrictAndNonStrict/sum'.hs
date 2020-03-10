module Main where

import Data.List (foldl')

-- Work in 0.32s
main = print $ foldl' (+) 0 [1..10000000]
