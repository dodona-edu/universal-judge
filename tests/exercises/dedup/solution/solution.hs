import Data.List (nub, sort)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let uniqueSorted = nub (sort args)
  putStrLn (unwords uniqueSorted)
