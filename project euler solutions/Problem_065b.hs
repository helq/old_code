--by yshhq

import Data.Char
import Data.Ratio

e = [2] ++ concat [ [1, 2*i, 1] | i <- [1..] ]

fraction [x] = x%1
fraction (x:xs) = x%1 + 1/(fraction xs)

result = sum $ map digitToInt $ show $ numerator $ fraction $ take 100 e 
