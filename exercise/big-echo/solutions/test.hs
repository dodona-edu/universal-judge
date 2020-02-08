
echo :: (Show b) => String -> b -> IO String
echo a b = do
    putStrLn (a ++ "-" ++ (show b))
    return (a ++ "-" ++ (show b))

main = do
    s <- getLine
    putStrLn s


echo2 a b = putStrLn (a ++ "-" ++ (show b))