-- by: helq

transpose          :: [[a]] -> [[a]]
transpose [xs]     = map (:[]) xs
transpose (xs:xss) = zipWith (:) xs $ transpose xss
transpose []       = []
