{-# LANGUAGE ForeignFunctionInterface #-}
 
main = do print "Hello from main"
          c_function
 
haskell_function i = print $ "Hello from haskell_function" ++ show i
 
foreign import ccall safe "prototypes.h"
    c_function :: IO ()
 
foreign export ccall
    haskell_function :: Int -> IO ()
