module Context${context_id} where

import qualified ${submission_name}
import Values
import System.IO (hPutStr, stderr)


main = do
    ## In Haskell we do the before/after inside the main (?)
    ${before}
    ## Call main function
    ${submission_name}.<%include file="function.mako" args="function=execution" />
    % for additional in additionals:
    hPutStr stderr "--${secret_id}-- SEP"
    putStr "--${secret_id}-- SEP"
    appendFile "${output_file}" "--${secret_id}-- SEP"
    v${loop.index} <- ${submission_name}.<%include file="function.mako" args="function=additional.input.function" />
    send v${loop.index} "${output_file}"
    % endfor
    ${after}
