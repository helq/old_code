-- by helq

isSort        :: Ord b => [b] -> Bool
isSort []     = True
isSort (x:xs) = and $ zipWith (<=) (x:xs) xs
