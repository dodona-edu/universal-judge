echoFile :: String -> IO String
echoFile d = do
    s <- readFile d
    return $ s
