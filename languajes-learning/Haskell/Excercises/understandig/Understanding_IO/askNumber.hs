import System.Random
import Control.Monad(when)
import System.IO
import Data.Char(isNumber,digitToInt)
  
main = do
    gen <- getStdGen

    hSetBuffering stdin NoBuffering
    askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
    let (randNumber, newGen) = randomR (0,9) gen :: (Int, StdGen)
    
    putStr "Which number in the range from 0 to 9 am I thinking of? "
    hFlush stdout

    numberChar <- getChar
    putChar '\n'

    when (isNumber numberChar) $ do
        let number = digitToInt numberChar
        if randNumber == number
            then putStrLn "You are correct!"
            else putStrLn $ "Sorry, it was " ++ show randNumber
        askForNumber newGen
