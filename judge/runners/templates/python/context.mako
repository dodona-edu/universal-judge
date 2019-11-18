## Code to execute one test context.
## The The first part of this file contains the evluation functions.
## The second part is responsible for actually running the tests.
<%! from testplan import BuiltinEvaluator, SpecificEvaluator %>

## Import the default handler for functions.
import values

## Output channel for evaluation results.
${secret_id}_file = open(r"${output_file}", "w")

def evaluated(result, expected, actual, messages=[]):
    values.send_evaluated(${secret_id}_file, result, expected, actual, messages)

def send(value):
    values.send_value(${secret_id}_file, value)

% for additional in additionals:
    def eval_${context_id}_${loop.index}(value):
        ${additional.value_code}
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
    eval_${context_id}_${loop.index}(${submission_name}.<%include file="function.mako" args="function=additional.function" />)
% endfor

${secret_id}_file.close()
