firstEven :: [Int] -> Maybe Int
firstEven xs = case filter even xs of
  [] -> Nothing
  (x : _) -> Just x

evens :: [Int] -> [Maybe Int]
evens xs = [Just x | x <- xs, even x]

nested :: Int -> Maybe (Maybe Int)
nested n
  | n > 0 = Just (Just n)
  | otherwise = Just Nothing

firstEvenIo :: [Int] -> IO (Maybe Int)
firstEvenIo xs = return (firstEven xs)
