import System.Environment (getArgs)
import Data.List (groupBy)
import Data.Char (isSpace)

main = do
    args <- getArgs
    case analizeArgs args of
        ("", _, _) -> putStrLn $ "mode of use: correctTimeASS " ++
                              "<file-ass-in> -d <delay> -o <file-ass-out>"
        (_, "", _) -> putStrLn "missing output file. Use `-o output.ass`"
        (inputFile, outputFile, delay) -> do
            file <- readFile inputFile
            let assModified = unlines $ map (addDelay delay) (lines file)
            writeFile outputFile assModified
            putStrLn "ok"

addDelay :: Time -> String -> String
addDelay delay line = case splitAt 10 line of
    ("Dialogue: ", params) -> "Dialogue: " ++ changeTime separatedParams
        where separatedParams = groupBy compByComma params
              compByComma ',' ',' = True
              compByComma x y | x == ',' || y == ',' = False
              compByComma _ _ = True
              changeTime (a:",":n:",":m:xs) = concat $ a:",":n':",":m':xs
                                where n' = show $ read n + delay
                                      m' = show $ read m + delay
    _ -> line

data Time = Time Int Int Int Int deriving (Eq, Ord)

instance Show Time where
    show (Time h m s ms) = show h ++ ":" ++ showT m ++ ":" ++ showT s ++ "." ++ showT ms
        where showT m = case show m of [x] -> ['0', x]; x -> x

instance Read Time where
    readsPrec n t = case dropWhile isSpace t of
        h    :':':m1:m2:':':s1:s2:'.':ms1:ms2:xs ->
            [(Time (read [h] :: Int)
                   (read [m1,m2] :: Int)
                   (read [s1,s2] :: Int)
                   (read [ms1,ms2] :: Int)
            , xs)]
        h1:h2:':':m1:m2:':':s1:s2:'.':ms1:ms2:xs ->
            [(Time (read [h1,h2] :: Int)
                   (read [m1,m2] :: Int)
                   (read [s1,s2] :: Int)
                   (read [ms1,ms2] :: Int)
            , xs)]
        xs -> case break (=='.') xs of
                (  "", '.':ms) -> [(Time 0 0 0 t, ys)]
                                    where (t, ys):_ = readsPrec n ms :: [(Int, String)]
                (secs, '.':ms) -> [(Time 0 0 (read secs :: Int) t, ys)]
                                    where (t, ys):_ = readsPrec n ms :: [(Int, String)]
                _ -> []

instance Num Time where
    m + n = fromInteger $ timeToInteger m + timeToInteger n
    m * n = fromInteger $ timeToInteger m * timeToInteger n
    negate m = fromInteger $ negate $ timeToInteger m
    abs m = fromInteger $ abs $ timeToInteger m
    signum m = fromInteger $ signum $ timeToInteger m
    fromInteger n = Time (fromInteger h :: Int)
                         (fromInteger m :: Int)
                         (fromInteger s :: Int)
                         (fromInteger ms :: Int)
        where (  h,  m) = sR  `divMod` 60
              ( sR,  s) = msR `divMod` 60
              (msR, ms) = n   `divMod` 100

timeToInteger (Time h m s ms) = (toInteger h)*60*60*100 + (toInteger $ (m*60 + s)*100 + ms)

analizeArgs :: [String] -> (FilePath, FilePath, Time)
analizeArgs (input:args) = extractArg args (input, "", 0)
    where extractArg ("-d":n  :args) (i,o,_) = extractArg args (i, o, read n)
          extractArg ("-o":str:args) (i,_,d) = extractArg args (i, str, d)
          extractArg [] exArgs = exArgs
analizeArgs [] = ("", "", 0)
