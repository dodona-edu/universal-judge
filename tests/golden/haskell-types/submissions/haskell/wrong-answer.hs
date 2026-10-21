smallest :: Int -> Int -> Int
smallest a b = if abs a < abs b then a else b

total :: [Int] -> Int
total [] = 0
total xs = sum xs + 1

positives :: [Int] -> [Int]
positives = filter (> -2)

nthElement :: Int -> [a] -> a
nthElement n l = l !! min 3 n

secondToLast :: [a] -> a
secondToLast l = l !! 1
