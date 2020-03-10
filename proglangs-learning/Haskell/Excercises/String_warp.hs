-- solution to http://langref.org/all-languages/strings/printing-strings/string-wrap
tenTimes = concat . replicate 10 $ "The quick brown fox jumps over the lazy dog. "
warp n = takeWhile (/="") . unfoldr (Just . splitAt n)
main = mapM_ (putStrLn . ("> " ++)) $ warp 77 tenTimes

main2 = mapM_ (putStrLn . ("> " ++)) $ takeWhile (/="") . unfoldr (Just . splitAt 77) . concat . replicate 10 $ "The quick brown fox jumps over the lazy dog. "
