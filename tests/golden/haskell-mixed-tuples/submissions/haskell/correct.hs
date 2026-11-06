swapPair :: (a, b) -> (b, a)
swapPair (a, b) = (b, a)

encode :: String -> [(Int, Char)]
encode [] = []
encode s@(c : _) = let (same, rest) = span (== c) s in (length same, c) : encode rest

decode :: [(Int, Char)] -> String
decode = concatMap (uncurry replicate)

record :: Int -> (Int, String, Bool, Double)
record n = (n, "five", True, 2.5)
