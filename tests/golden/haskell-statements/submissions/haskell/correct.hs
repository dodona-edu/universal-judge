import Data.List (sortBy)
import Data.Ord (comparing, Down (..))

sortDesc :: [Int] -> [Int]
sortDesc = sortBy (comparing Down)

double :: Int -> Int
double = (* 2)
