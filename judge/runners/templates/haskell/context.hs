module Context${context_id} where

import qualified ${name}
import Values
import System.IO (hPutStr, stderr)


main = do
    ## In Haskell we do the before/after inside the main (?)
    ${before}
    ## Call main function
    ${name}.<%include file="function.mako" args="function=execution" />
    % for additional in additionals:
    hPutStr stderr "--${code_identifier}-- SEP"
    putStr "--${code_identifier}-- SEP"
    appendFile "${output_file}" "--${code_identifier}-- SEP"
    v${loop.index} <- ${name}.<%include file="function.mako" args="function=additional.input.function" />
    send v${loop.index} "${output_file}"
    % endfor
    ${after}
