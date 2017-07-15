module Main where

import Data.List (foldr1)

mainOrig = do
    f <- readFile "067-triangle.txt"
    let trian = map (map (read::String -> Int) . words) $ lines f
        sol = head . foldr1 maxOfSumPairs
    print $ sol trian

maxOfSumPairs xs ys = zipWith (+) xs $ zipWith max ys (tail ys)

-- ///////////////////////
-- For funny

main = do
    f <- readFile "067-triangle.txt"
    let trian = map (map (read::String -> Int) . words) $ lines f
        (n,bs) = solPath trian
    print n
    putStrLn $ pathToStr bs


data Branch = LB | RB
    deriving (Eq, Show)

-- maxOfSumPairsPath
mOSPPath :: Integral a => [a] -> [(a, [Branch])] -> [(a, [Branch])]
mOSPPath xs ys = zipWith addToBr xs $ zipWith maxBr ys (tail ys)
         where addToBr x (y,bs) = (x+y,bs)
               maxBr (x,bsL) (y,bsR) | x > y     = (x,LB:bsL)
                                     | otherwise = (y,RB:bsR)

-- Solution with path
solPath :: Integral a =>  [[a]] -> (a, [Branch])
solPath = head . foldr mOSPPath [(0,[])|_<-[1..]]


-- ///////////////////////
-- for more funny, printing branch

totalWeigthLB    :: Integral a => [Branch] -> a
totalWeigthLB bs = maximum $ 0 : sumIterate 0 bs
  where sumIterate _ [] = []
        sumIterate t (b:bs)
            | b == LB   = t' : sumIterate t' bs
            | otherwise = sumIterate (t-1) bs
                where t' = t+1

ifLB b yes no = if b == LB then yes else no

pathToStr    :: [Branch] -> String
pathToStr bs = f (totalWeigthLB bs) bs
    where f _ []     = ""
          f l (b:bs) = addSpaces l b ++ "\n" ++ f (l + ifLB b (-1) 1) bs
          addSpaces l b = [' '|_<-[1..l]] ++ [ifLB b '/' '\\']

