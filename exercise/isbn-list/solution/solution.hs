{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}

import Data.Char
import Control.Monad
import Data.Maybe

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

checkLength :: String -> Maybe String
checkLength x
    | length(x) == 10 = Just x
    | otherwise = Nothing


areDigits :: [Char] -> Bool
areDigits l = all isDigit l

asDigit :: Char -> Int
asDigit 'X' = 10
asDigit 'x' = 10
asDigit x   = digitToInt x


checkDigits :: String -> Maybe String
checkDigits x = if (areDigits (slice 0 10 x)) then Just x else Nothing

checkChecksum :: String -> Maybe String
checkChecksum s =
    let checksum = (sum $ map (\(a, b) -> a * (asDigit b)) (zip [1..] (slice 0 10 s))) `mod` 11
    in if (checksum ) == asDigit (s !! 9)
       then Just s
       else Nothing

isbn10check :: String -> Maybe String
isbn10check = return >=> checkLength >=> checkDigits >=> checkChecksum


isIsbn10 :: String -> Bool
isIsbn10 = isJust . isbn10check


isIsbn a b = if b then isIsbn10 a else False
areIsbn a b = map (\x -> (isIsbn x b)) a


main = do
    putStrLn $ show (areIsbn ["0012345678", "0012345679", "9971502100", "080442957X", "The Practice of Computing Using Python", "9789027439642", "5486948320146"] True)