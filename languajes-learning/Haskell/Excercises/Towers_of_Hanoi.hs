module Main where

-- Code by: helq
-- Date: 2012

main = print $ hanoi 24

hanoi n = h [n,n-1..1::Int] [] []

h []     _  _  = [([],[],[])]
h (x:xs) ys zs = map (\(xi,zi,yi)->(x:xi,yi,zi)) h1
                 ++
                 map (\(yi,xi,zi)->(xi,yi,x:zi)) h1
    where h1 = h xs ys zs