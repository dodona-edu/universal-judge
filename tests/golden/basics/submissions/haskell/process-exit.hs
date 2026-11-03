import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO.Unsafe (unsafePerformIO)

add :: Int -> Int -> Int
add a b = a + b

greet :: String -> String
greet name = unsafePerformIO (exitWith (ExitFailure 1))

printGreeting :: String -> IO ()
printGreeting name = putStrLn ("Hello, " ++ name ++ "!")
