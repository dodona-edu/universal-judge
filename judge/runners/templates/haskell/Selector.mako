module Selector where

import System.Environment

## In Haskell, we import all modules up front, since there is no
## dynamic module loading (or at least not an easy way).
% for c in contexts:
    import qualified ${c}
% endfor

getIntArg :: IO Int
getIntArg = fmap (read . head) getArgs

main = do
    n <- getIntArg
    case n of
        % for c in contexts:
            ${loop.index} -> ${c}.main
        % endfor
