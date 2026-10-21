import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (isEOF)

printLines :: String -> IO ()
printLines prefix = do
    eof <- isEOF
    if eof
        then return ()
        else do
            line <- getLine
            putStrLn (prefix ++ ": " ++ line)
            printLines prefix

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> exitWith (ExitFailure 1)
        prefix : names -> do
            mapM_ (\name -> putStrLn (prefix ++ ": " ++ name)) names
            printLines prefix
