## Code to execute one test context.
## The The first part of this file contains the evluation functions.
## The second part is responsible for actually running the tests.
<%! from testplan import BuiltinEvaluator %>

## Import the default handler for functions.
import values as ${code_identifier}_values

## Output channel for evaluation results.
${code_identifier}_file = open(r"${output_file}", "w")

% for additional in additionals:
    def eval_${context_id}_${loop.index}(value):
        % if isinstance(additional.output.result.evaluator, BuiltinEvaluator):
            ${code_identifier}_values.send(${code_identifier}_file, value)
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
import ${name}

## Handle test cases
% for additional in additionals:
    sys.stderr.write("--${code_identifier}-- SEP")
    sys.stdout.write("--${code_identifier}-- SEP")
    ${code_identifier}_file.write("--${code_identifier}-- SEP")
    eval_${context_id}_${loop.index}(${name}.<%include file="function.mako" args="function=additional.input.function" />)
% endfor

${code_identifier}_file.close()
