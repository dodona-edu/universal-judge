firstEven :: [Int] -> Maybe Int
firstEven xs = case filter even xs of
  [] -> Just 7
  (x : _)
    | x > 5 -> Nothing
    | otherwise -> Just x

evens :: [Int] -> [Maybe Int]
evens xs = [Just x | x <- xs, even x] ++ [Nothing]

nested :: Int -> Maybe (Maybe Int)
nested n
  | n > 0 = Just Nothing
  | otherwise = Just (Just n)

firstEvenIo :: [Int] -> IO (Maybe Int)
firstEvenIo xs = return (firstEven xs)
