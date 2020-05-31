{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}

import Data.Char
import Control.Monad
import Data.Maybe

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

checkLength :: Int -> String -> Maybe String
checkLength l x
    | length(x) == l = Just x
    | otherwise = Nothing


areDigits :: [Char] -> Bool
areDigits l = all isDigit l

asDigit :: Char -> Int
asDigit 'X' = 10
asDigit 'x' = 10
asDigit x   = digitToInt x


checkDigits :: String -> Maybe String
checkDigits x = if (areDigits (slice 0 8 x)) then Just x else Nothing

checkAllDigits :: String -> Maybe String
checkAllDigits x = if (areDigits x) then Just x else Nothing

checkChecksum10 :: String -> Maybe String
checkChecksum10 s =
    let checksum = (sum $ map (\(a, b) -> a * (asDigit b)) (zip [1..] (slice 0 8 s))) `mod` 11
    in if checksum == asDigit (s !! 9)
       then Just s
       else Nothing


odds :: [a] -> [a]
odds [] = []
odds [x] = []
odds (e1:e2:xs) = e2 : odds xs

evens :: [a] -> [a]
evens [] = []
evens [x] = [x]
evens (e1:e2:xs) = e1 : evens xs


checkChecksum13 :: String -> Maybe String
checkChecksum13 s =
    let e = sum $ map asDigit (odds (slice 0 11 s))
        o = sum $ map asDigit (evens (slice 0 11 s))
        x13 = (10 - (o + 3*e) `mod` 10) `mod` 10
    in if x13 == asDigit (s !! 12)
        then Just s
        else Nothing


isbn10check :: String -> Maybe String
isbn10check = return >=> checkLength 10 >=> checkDigits >=> checkChecksum10


isIsbn10 :: String -> Bool
isIsbn10 = isJust . isbn10check

isbn13check :: String -> Maybe String
isbn13check = return >=> checkLength 13 >=> checkAllDigits >=> checkChecksum13

isIsbn13 :: String -> Bool
isIsbn13 = isJust . isbn13check



isIsbn a b = if b then isIsbn13 a else isIsbn10 a
areIsbn a b = map (\x -> (isIsbn x b)) a


main = do
    putStrLn $ show (areIsbn ["0012345678", "0012345679", "9971502100", "080442957X", "The Practice of Computing Using Python", "9789027439642", "5486948320146"] True)