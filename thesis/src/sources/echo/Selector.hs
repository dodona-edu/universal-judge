module Selector where
import System.Environment
import qualified Context00
import qualified Context01
main = do
    [n] <- getArgs
    case n of
        "Context00" -> Context00.main
        "Context01" -> Context01.main