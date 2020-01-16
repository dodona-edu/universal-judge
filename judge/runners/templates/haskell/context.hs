## Code to execute one test context.
<%! from testplan import Assignment %>
module Context where

import System.IO (hPutStr, stderr)
import System.Environment

import qualified ${submission_name}
import qualified Evaluator


main = do
    ${before}

    % if main_testcase.exists:
        let mainArgs = [\
            % for argument in main_testcase.arguments:
                <%include file="value.mako" args="value=argument"/>\
            % endfor
        ]
        withArgs mainArgs ${submission_name}.main
        hPutStr stderr "--${secret_id}-- SEP"
        putStr "--${secret_id}-- SEP"
        Evaluator.writeDelimiter "--${secret_id}-- SEP"
    % endif

    % for additional in additional_testcases:
        % if isinstance(additional.statement, Assignment):
            <%include file="assignment.mako" args="assignment=additional.statement,root=False" />
        % else:
            % if additional.has_return:
                v${loop.index} <- <%include file="function.mako" args="function=additional.statement" />
                Evaluator.v_evaluate_${loop.index} v${loop.index}
            % else:
                <%include file="function.mako" args="function=additional.statement" />
            % endif
        % endif

        hPutStr stderr "--${secret_id}-- SEP"
        putStr "--${secret_id}-- SEP"
        Evaluator.writeDelimiter "--${secret_id}-- SEP"
    % endfor
    ${after}
