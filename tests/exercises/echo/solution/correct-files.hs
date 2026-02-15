import System.IO
import Control.Exception (catch, SomeException)
import Data.List (dropWhileEnd)
import Data.Char (isSpace)

trimEnd :: String -> String
trimEnd = dropWhileEnd isSpace

main :: IO ()
main = do
    input <- getLine
    exists <- doesFileExist "input2.txt"
    if exists
        then do
            fileTxtRaw <- readFile "input2.txt"
            let fileTxt = trimEnd fileTxtRaw
            if not (null fileTxt)
                then putStrLn fileTxt
                else putStrLn input
        else putStrLn input

doesFileExist :: FilePath -> IO Bool
doesFileExist name = catch (do
    handle <- openFile name ReadMode
    hClose handle
    return True) (\e -> let _ = (e :: SomeException) in return False)
