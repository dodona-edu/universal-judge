module Selector where

import System.Environment

## In Haskell, we import all modules up front, since there is no
## dynamic module loading (or at least not an easy way).
% for c in contexts:
    import qualified ${c}
% endfor

main = do
    [n] <- getArgs
    case n of
        % for c in contexts:
            "${c}" -> ${c}.main
        % endfor
