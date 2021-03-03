isValidPassword:: [Char] -> [Char] -> Bool
isValidPassword passwd chk = let (x,y,z) = parse $ split $ chk
                             in check (x - 1) (y - 1) z passwd

check:: Int -> Int -> String -> String -> Bool
check x y a str = xor ([str !! x] == a) ([str !! y] == a)

parse :: [String] -> (Int,Int,String)
parse (x:y:z:_) = ((read x::Int), (read y::Int),z)

split :: String -> [String]
split str = let rpl '-' = ' '
                rpl c = c
            in words $ map rpl str

xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a
