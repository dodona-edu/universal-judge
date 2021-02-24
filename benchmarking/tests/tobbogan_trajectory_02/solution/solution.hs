import System.IO.Unsafe

skip :: Int -> [a] -> [a]
skip n xs@(x:_) = x : (skip n $ drop n xs)
skip _ _        = []

countTrees :: Int -> Int -> String -> Int
countTrees n m = length . filter (=='#') . zipWith (flip (!!)) [0,n..] . map cycle . skip m . lines . unsafePerformIO . readFile
