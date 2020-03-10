-- Code by: helq
-- Date: 15-04-12 15:00

c :: Int -> [String]
c n
    | n==0      = [""]
    | otherwise = map ('0':) cm ++ map ('1':) (reverse cm)
    where
		cm = c (n-1)
