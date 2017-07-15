module Main where

perms        :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concatMap bet $ perms xs
    where bet []     = [[x]]
          bet (y:ys) = (x:y:ys) : (map (y:) $ bet ys)

 -- Mantiene el orden relativo, si el conjunto tiene los elementos
 -- ordenados y no repetidos en orden ascendente, entonces el conjunto resultante
 -- está ordenado lexicográficamente.
perms2 [] = [[]]
perms2 xs = concatMap (\(a,as)->map (a:) (perms2 as)) (elems xs)
    where elems []     = []
          elems (a:as) = (a,as): map (\(a',as')->(a',a:as')) (elems as)

--perms2        :: [a] -> [[a]]
--perms2 []     = [[]]
--perms2 (x:xs) = bet (perms xs)
--    where bet xss@([]:_) = map (x:) xss
--          bet xss        = map (x:) xss ++ zipWith (:) (cycle $ map head xss)
--                                                       (bet (map tail xss))

intersperse :: a -> [[a]] -> [a]
intersperse k ys = inter ys
    where inter [y]    = y
          inter (y:ys) = y ++ k : inter (ys)
          inter []     = []

main = putStrLn $ intersperse '\n' $ perms ['a'..'l']