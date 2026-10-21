smallest :: Int -> Int -> Int
smallest a b = if a < b then a else b

total :: [Int] -> Int
total = sum

positives :: [Int] -> [Int]
positives = filter (> 0)

nthElement :: Int -> [a] -> a
nthElement n l = l !! n

secondToLast :: [a] -> a
secondToLast = last . init
