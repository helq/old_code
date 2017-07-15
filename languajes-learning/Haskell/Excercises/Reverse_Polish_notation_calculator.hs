
{-# LANGUAGE RankNTypes, ImpredicativeTypes #-}

type Operation = Fractional a => [a] -> [a]

solveRPN :: (Fractional a, Read a) => String -> a
solveRPN = head . solve [] . words

solve :: (Fractional a, Read a) => [a] -> [String] -> [a]
solve ns []     = ns
solve ns (s:ss) = 
    case isOperator s of
        Just ope -> solve (ope ns) ss
        Nothing  -> solve ( read s : ns) ss

isOperator :: String -> Maybe Operation
isOperator o = lookup o operators

operators :: [(String, Operation)]
operators = [ ("+", op (+)),
              ("-", op (-)),
              ("*", op (*)),
              ("/", op (/))
            ]
--op :: (Integer -> Integer -> Integer) -> Operation
op f (n:n':ns) = n' `f` n : ns

main = print $ solveRPN "10 4 3 + 2 * - 7 /"
                    -- ( 10 - ((4+3)*2) ) / 7
