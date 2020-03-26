## Code to execute_module one test context.
<%! from tested.serialisation import Statement, Expression %>
<%! from tested.utils import get_args %>
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


% for additional in testcases:
    v_evaluate_${loop.index} value = <%include file="expression.mako" args="expression=additional.value_function"/>
% endfor


main = do
    % if before:
        ${before}
    % endif

    % if context_testcase.exists:
        let mainArgs = [\
            % for argument in context_testcase.arguments:
                <%include file="value.mako" args="value=argument"/>\
            % endfor
        ]
        withArgs mainArgs ${submission_name}.main
    % endif
    hPutStr stderr "--${secret_id}-- SEP"
    putStr "--${secret_id}-- SEP"
    writeDelimiter value_file "--${secret_id}-- SEP"
    writeDelimiter exception_file "--${secret_id}-- SEP"

    % for additional in testcases:
        % if isinstance(additional.command, get_args(Statement)):
            <%include file="statement.mako" args="statement=additional.command,root=False" />
        % else:
            % if additional.has_return:
                v${loop.index} <- <%include file="expression.mako" args="expression=additional.command,lifting=True" />
                v_evaluate_${loop.index} v${loop.index}
            % else:
                <%include file="expression.mako" args="expression=additional.command" />
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
