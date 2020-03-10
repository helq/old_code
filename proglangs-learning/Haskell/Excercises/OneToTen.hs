-- by helq
import System.IO (hWaitForInput, stdin)


data Number = Number [String]
instance Show Number where
    show (Number num) = concatMap (++"\n") num

(+:) :: Number -> Number -> Number
(Number n1) +: (Number n2) = Number $ zipWith (++) n1 n2

toNum :: Int -> Number
toNum n = foldr (\x y->digToNum x +: y) emptyNum (show n)
    where
        emptyNum = Number [ ""|_<-[1..]]

        digToNum :: Char -> Number
        digToNum n = case n of
            '0' -> _0
            '1' -> _1
            '2' -> _2
            '3' -> _3
            '4' -> _4
            '5' -> _5
            '6' -> _6
            '7' -> _7
            '8' -> _8
            '9' -> _9

_1 = Number
     ["  ##   "
    , "####   "
    , "  ##   "
    , "  ##   "
    , "  ##   "
    , "  ##   "
    , "###### "]

_2 = Number
     [" #######  "
    , "##     ## "
    , "       ## "
    , " #######  "
    , "##        "
    , "##        "
    , "######### "]

_3 = Number
     [" #######  "
    , "##     ## "
    , "       ## "
    , " #######  "
    , "       ## "
    , "##     ## "
    , " #######  "]

_4 = Number
     ["##        "
    , "##    ##  "
    , "##    ##  "
    , "##    ##  "
    , "######### "
    , "      ##  "
    , "      ##  "]

_5 = Number
     ["######## "
    , "##       "
    , "##       "
    , "#######  "
    , "      ## "
    , "##    ## "
    , " ######  "]

_6 = Number
     [" #######  "
    , "##     ## "
    , "##        "
    , "########  "
    , "##     ## "
    , "##     ## "
    , " #######  "]


_7 = Number
     ["######## "
    , "##    ## "
    , "    ##   "
    , "   ##    "
    , "  ##     "
    , "  ##     "
    , "  ##     "]

_8 = Number
     [" #######  "
    , "##     ## "
    , "##     ## "
    , " #######  "
    , "##     ## "
    , "##     ## "
    , " #######  "]

_9 = Number
     [" #######  "
    , "##     ## "
    , "##     ## "
    , " ######## "
    , "       ## "
    , "##     ## "
    , " #######  "]

_0 = Number
     ["  #####   "
    , " ##   ##  "
    , "##     ## "
    , "##     ## "
    , "##     ## "
    , " ##   ##  "
    , "  #####   "]


prettyShow :: Number -> String
prettyShow (Number num) =
            newLineBefore ++
            concatMap (\a->spacesBefore ++ a ++ "\n") num ++
            newLineAfter
    where
        spacesBefore  = [ ' ' |_<-[1..44]]
        newLineBefore = [ '\n'|_<-[1..100]]
        newLineAfter  = [ '\n'|_<-[1..6]]

-- Main here
main = printNums 1 10

-- TODO: don't use hWaitForInput
printNums a b
    | a > b = hWaitForInput stdin (-1) >> return ()
    | otherwise = do
        putStrLn $ prettyShow $ toNum a
        hWaitForInput stdin 1000
        printNums (a+1) b

