import System.IO.Unsafe

countTrees :: String -> Int
countTrees = length . filter (=='#') . zipWith (flip (!!)) [0,3..] . map cycle . lines . unsafePerformIO . readFile
