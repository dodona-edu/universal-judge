import Control.Exception (ErrorCall (..), throwIO)
import Data.List (sortBy)
import Data.Ord (comparing, Down (..))

sortDesc :: [Int] -> [Int]
sortDesc = sortBy (comparing Down)

double :: Int -> Int
double = (* 2)

boom :: Int -> Int
boom _ = error "boom"

boomIO :: IO Int
boomIO = throwIO (ErrorCall "boomIO")
