printGreeting :: String -> IO ()
printGreeting name = putStrLn (spin (length name))
  where
    spin :: Int -> String
    spin n = spin $! n + 1
