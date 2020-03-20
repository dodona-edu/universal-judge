## Code to execute one test context.
<%! from testplan import Assignment %>
module ${context_name} where

import System.IO (hPutStr, stderr)
import System.Environment
import Values
import Control.Monad.Trans.Class

## Import the language specific evaluators we will need.
% for name in evaluator_names:
    import qualified ${name}
% endfor

import qualified ${submission_name}


value_file = "${value_file}"
exception_file = "${exception_file}"

writeDelimiter :: FilePath -> String -> IO ()
writeDelimiter = appendFile


send :: Typeable a => a -> IO ()
send = sendValue value_file


% for additional in additional_testcases:
    v_evaluate_${loop.index} value = <%include file="function.mako" args="function=additional.value_function"/>
% endfor


main = do
    % if before:
        ${before}
    % endif

    % if main_testcase.exists:
        let mainArgs = [\
            % for argument in main_testcase.arguments:
                <%include file="value.mako" args="value=argument"/>\
            % endfor
        ]
        withArgs mainArgs ${submission_name}.main
        hPutStr stderr "--${secret_id}-- SEP"
        putStr "--${secret_id}-- SEP"
        writeDelimiter value_file "--${secret_id}-- SEP"
        writeDelimiter exception_file "--${secret_id}-- SEP"
    % endif

    % for additional in additional_testcases:
        % if isinstance(additional.statement, Assignment):
            <%include file="assignment.mako" args="assignment=additional.statement,root=False" />
        % else:
            % if additional.has_return:
                v${loop.index} <- <%include file="function.mako" args="function=additional.statement" />
                v_evaluate_${loop.index} v${loop.index}
            % else:
                <%include file="function.mako" args="function=additional.statement" />
            % endif
        % endif

        hPutStr stderr "--${secret_id}-- SEP"
        putStr "--${secret_id}-- SEP"
        writeDelimiter value_file "--${secret_id}-- SEP"
        writeDelimiter exception_file "--${secret_id}-- SEP"
    % endfor

    % if after:
        ${after}
    % endif
