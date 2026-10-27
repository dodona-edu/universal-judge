import System.Exit

check :: Int -> IO ()
check n = exitWith (ExitFailure n)
