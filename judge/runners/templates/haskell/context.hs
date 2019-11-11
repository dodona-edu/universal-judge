module Context${context_id} where

import qualified ${submission_name}
import Values (send, evaluated)
import System.IO (hPutStr, stderr)

## Create custom evaluation functions
% for additional in additionals:
    eval_${context_id}_${loop.index} output value = ${additional.output.value_code}
% endfor


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
    eval_${context_id}_${loop.index} "${output_file}" v${loop.index}
    % endfor
    ${after}
