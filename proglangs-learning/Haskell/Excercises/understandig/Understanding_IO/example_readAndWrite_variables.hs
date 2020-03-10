import Data.IORef
main = do varA <- newIORef 0  -- Create and initialize a new variable
          let varB = varA
          a0 <- readIORef varA
          writeIORef varB 1
          a1 <- readIORef varA
          print (a0, a1, a0==a1, varA==varB)
