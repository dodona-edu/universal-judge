getOccurencesInString :: String -> Char -> Int
getOccurencesInString text search = length [x | x <- text, x == search]

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

isValidPassword :: String -> String -> Bool
isValidPassword password requirements = do
    getOccurencesInString password (last requirements) >= (read (head (wordsWhen (=='-') (unwords (take 1 (wordsWhen (==' ') requirements)))))::Int)
    && (getOccurencesInString password (last requirements) <= (read (last (wordsWhen (=='-') (unwords (take 1 (wordsWhen (==' ') requirements)))))::Int))
