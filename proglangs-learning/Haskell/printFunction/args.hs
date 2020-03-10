import System.Environment (getArgs)
import Data.Maybe (listToMaybe)

error_mesage = "Incorrect number of parameters\n\
               \  use: args <pages> <register>\n\n\
               \pages must be powers of 2 between 1024 and 16384\n\
               \a register must be an positive integer of 32bits"

main = do
    args <- getArgs
    case (map maybeRead args) :: [Maybe Integer] of
        [Just pageSize, Just offset] ->
            case (checkPageSize pageSize, check32BitsNum offset) of
                (True, True) -> do
                    let (page, align) = offset `quotRem` pageSize
                    {- print "nutearn" -}
                    putStrLn $ "The address for " ++ (show offset) ++ " is:"
                    putStrLn $ "Page number: " ++ (show page)
                    putStrLn $ "Alignment: " ++ (show align)
                (False, _)   -> putStrLn "Invalid size of page, page must be \
                                        \a power of 2 between 1024 16384"
                _            -> putStrLn "Invalid number of offset, \
                                        \offset 0 ≤ offset < 2^32"
        _ -> putStrLn error_mesage

checkPageSize :: Integer -> Bool
checkPageSize n = any ((==0).(n -)) $ take 5 $ iterate (*2) 1024

check32BitsNum :: Integer -> Bool
check32BitsNum n = 0 <= n && n < 2^32

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
