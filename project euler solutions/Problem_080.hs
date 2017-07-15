module Main where
import Data.List

minMul m n = f 1
    where f 10 = 9
          f i
            | q > n = i-1
            | otherwise = f (i+1)
               where q = (m*10+i)*i

sqrtRoot100 :: Integral a => a -> [a]
sqrtRoot100 n = f 0 n
    where f m n = q : f (p+q) ((n-p*q)*100)
            where q = minMul m n
                  p = (m*10+q)

main = do
    let squares = [x^2|x<-[1..10]]
        sumSqrt = sum $ map (sum.(take 100).sqrtRoot100) $ [1..100] \\ squares
    putStrLn $ show $ sumSqrt