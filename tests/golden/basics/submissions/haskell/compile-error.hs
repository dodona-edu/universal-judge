add :: Int -> Int -> Int
add a b = a + b

greet :: String -> String
greet name = "Hello, " ++ nme ++ "!"

printGreeting :: String -> IO ()
printGreeting name = putStrLn ("Hello, " ++ name ++ "!")
