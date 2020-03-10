import System.Environment
import System.IO
import System.IO.Error
import Control.Exception.Base (catch)
import GHC.IO.Exception (ioe_filename)

main = toTry `catchIOError` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

--handler :: IOError -> IO ()
handler e = putStrLn $ "Whoops, had some trouble! " ++ show (ioe_filename e)
