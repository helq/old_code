module Main where

main = print $ sum $ takeWhile (<4000000) $ [x|x<-fib 1 2, even x]
    where fib a b = a : fib b (a+b)
