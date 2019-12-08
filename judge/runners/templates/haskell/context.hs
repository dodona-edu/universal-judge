## Code to execute one test context.
module Context where

import System.IO (hPutStr, stderr)
import System.Environment

import qualified ${submission_name}
import qualified Evaluator


main = do
    ## In Haskell we do the before/after inside the main (?)
    ${before}
    ## Call main function
    % if main_testcase.exists:
        let mainArgs = [\
            % for argument in main_testcase.arguments:
                <%include file="value.mako" args="value=argument"/>\
            % endfor
        ]
        withArgs mainArgs ${submission_name}.main
    % endif

    % for additional in additional_testcases:
        hPutStr stderr "--${secret_id}-- SEP"
        putStr "--${secret_id}-- SEP"
        Evaluator.writeDelimiter "--${secret_id}-- SEP"
        % if additional.has_return:
            v${loop.index} <- ${submission_name}.<%include file="function.mako" args="function=additional.function" />
            Evaluator.v_evaluate_${loop.index} v${loop.index}
        % else:
            ${submission_name}.<%include file="function.mako" args="function=additional.function" />
        % endif

    % endfor
    ${after}
