rev :: [a] -> [a]
rev = reverse

flatten :: [[a]] -> [a]
flatten = concat

firstOr :: [a] -> Maybe a
firstOr [] = Nothing
firstOr (x : _) = Just x

actions :: [IO Int]
actions = [return 1, return 2]
