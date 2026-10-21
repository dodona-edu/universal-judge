-- Greeting program
{-# LANGUAGE LambdaCase #-}

main :: IO ()
main = mapM_ (\case
    1 -> putStrLn "Hello"
    _ -> putStrLn "Other") [1 :: Int]
