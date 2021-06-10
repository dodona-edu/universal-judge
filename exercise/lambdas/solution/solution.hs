add :: Int -> Int -> Int
add a b = a + b

apply :: (Int -> Int -> Int) -> Int -> Int -> Int
apply f a b = f a b
