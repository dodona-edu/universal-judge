repair :: [Int] -> Int
repair l = (take 1 [x * y * z | x <- l, y <- l, z <- l, x + y + z == 2020]) !! 0
