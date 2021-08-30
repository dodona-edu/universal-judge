cirkels :: String -> Int
cirkels []     = 0
cirkels ('8':xs) = 2 + cirkels xs
cirkels ('0':xs) = 1 + cirkels xs
cirkels ('4':xs) = 1 + cirkels xs
cirkels ('6':xs) = 1 + cirkels xs
cirkels ('9':xs) = 1 + cirkels xs
cirkels (_:xs)   = cirkels xs

main = do
    s <- getLine
    let result = cirkels s
    print result