add :: Int -> Int -> Int
add a b = a + b

greet :: String -> String
greet name = error ("cannot greet " ++ name)

printGreeting :: String -> IO ()
printGreeting name = putStrLn ("Hello, " ++ name ++ "!")
