import System.Random
import Control.Monad (forM_, when)
import Data.IORef

main = do
    gen <- getStdGen
    putStr $ take 20 (randomRs ('a','z') gen) ++ "\n"
    gen2 <- newStdGen
    putStr $ take 20 (randomRs ('a','z') gen2) ++ "\n"
    gen3 <- getStdGen
    putStr $ take 20 (randomRs ('a','z') gen3) ++ "\n"
    gen4 <- getStdGen
    putStr $ take 20 (randomRs ('a','z') gen4) ++ "\n"
    (randomIO :: IO Int) >>= print
    (randomIO :: IO Int) >>= print


    -- mode imperative emulating 

    a <- newIORef 0

    forM_ [1..100] $ \i-> do
        j <- readIORef a
        writeIORef a (i+j)

    readIORef a >>= print


    -- emulating mode imperative 

    writeIORef a 0
    i <- newIORef 0

    while (app i (<=) 101) $ do 
        iCont <- readIORef i
        j <- readIORef a
        writeIORef a (iCont+j)
        writeIORef i (iCont+1)

    readIORef a >>= print


while :: IO Bool -> IO () -> IO ()
while b action = do
    result <- b
    when result $ do
        action
        while b action

app xRef f y = readIORef xRef >>= return . (`f` y)
