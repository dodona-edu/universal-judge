import System.IO
import System.Directory (doesFileExist)

load :: FilePath -> IO String
load filename = do
    exists <- doesFileExist filename
    if exists
        then do
            contents <- readFile filename
            -- Grab the first line, defaulting to empty string if file is empty
            return $ if null (lines contents) then "" else head (lines contents)
        else return ""

save :: String -> FilePath -> IO ()
save user filename = writeFile filename (user ++ "\n")

main :: IO ()
main = do
    user <- load "datafile.txt"
    if null user
        then do
            putStrLn "Hello, I don't believe we have met."
            hFlush stdout  -- Ensure the prompt prints before asking for input
            newName <- getLine
            save newName "datafile.txt"
            putStrLn $ "Nice to meet you " ++ newName ++ "."
        else
            putStrLn $ "It's good to see you again, " ++ user ++ "."
