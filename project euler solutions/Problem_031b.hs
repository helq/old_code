-- "Solution by glguy"

main :: IO ()
main = print $ count [200,100,50,20,10,5,2,1] 200

-- only decremental lists
count :: [Integer] -> Integer -> Integer
count _ 0      = 1
count [c] _    = 1
count (c:cs) s = sum $ map (count cs . (s-)) [0,c..s]