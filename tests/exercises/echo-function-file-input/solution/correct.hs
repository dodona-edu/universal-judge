strip = lstrip . rstrip
lstrip = dropWhile (`elem` " \t\n")
rstrip = reverse . lstrip . reverse

echoFile :: String -> IO String
echoFile d = do
    s <- readFile d
    return $ strip s
