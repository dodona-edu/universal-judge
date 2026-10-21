import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPutStrLn, isEOF, stderr)

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
    hPutStrLn stderr ("debug: " ++ show (length args) ++ " arguments")
    case args of
        [] -> exitWith (ExitFailure 1)
        prefix : names -> do
            mapM_ (\name -> putStrLn (prefix ++ ": " ++ name)) names
            printLines prefix
