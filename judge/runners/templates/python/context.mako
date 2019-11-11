## Code to execute one test context.
## The The first part of this file contains the evluation functions.
## The second part is responsible for actually running the tests.
<%! from testplan import BuiltinEvaluator %>

## Import the default handler for functions.
import values as ${secret_id}_values

## Output channel for evaluation results.
${secret_id}_file = open(r"${output_file}", "w")

% for additional in additionals:
    def eval_${context_id}_${loop.index}(value):
        % if isinstance(additional.output.result.evaluator, BuiltinEvaluator):
            ${secret_id}_values.send(${secret_id}_file, value)
        % endif
% endfor

import sys

## Set the main arguments.
sys.argv.extend([\
% for argument in execution.arguments:
    <%include file="argument.mako" args="argument=argument"/>\
% endfor
])

## Import the code for the first time
import ${submission_name}

## Handle test cases
% for additional in additionals:
    sys.stderr.write("--${secret_id}-- SEP")
    sys.stdout.write("--${secret_id}-- SEP")
    ${secret_id}_file.write("--${secret_id}-- SEP")
    eval_${context_id}_${loop.index}(${submission_name}.<%include file="function.mako" args="function=additional.input.function" />)
% endfor

${secret_id}_file.close()
