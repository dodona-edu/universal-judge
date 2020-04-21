## Code to execute_module one test context.
<%! from tested.languages.generator import _TestcaseArguments %>
<%! from tested.serialisation import Statement, Expression %>
<%! from tested.utils import get_args %>
import values
import sys

##################################
## Setup                        ##
##################################

## Import the language specific evaluators we will need.
% for name in evaluator_names:
    import ${name}
% endfor


## Open the files to which we write results.
value_file = open(r"${value_file}", "w")
exception_file = open(r"${exception_file}", "w")

## Write the delimiter and flush to ensure the output is in the files.
## This is necessary, otherwise the delimiters are sometimes missing when
## execution is killed due to timeouts.
def write_delimiter(delimiter):
    value_file.write(delimiter)
    exception_file.write(delimiter)
    sys.stderr.write(delimiter)
    sys.stdout.write(delimiter)
    sys.stdout.flush()
    sys.stderr.flush()
    value_file.flush()
    exception_file.flush()


##################################
## Predefined functions         ##
##################################

## Send a value to TESTed.
def send(value):
    values.send_value(value_file, value)

## Send an exception to TESTed.
def send_exception(exception):
    values.send_exception(exception_file, exception)

## Send the result of a language specific value evaluator to TESTed.
def send_specific_value(r):
    values.send_evaluated(value_file, r.result, r.readable_expected, r.readable_actual, r.messages)

## Send the result of a language specific exception evaluator to TESTed.
def send_specific_exception(r):
    values.send_evaluated(exception_file, r.result, r.readable_expected, r.readable_actual, r.messages)


##################################
## Main testcase evalutors      ##
##################################
def e_evaluate_main(value):
    <%include file="statement.mako" args="statement=context_testcase.exception_function"/>

##################################
## Other testcase evaluators    ##
##################################
% for testcase in testcases:
    % if testcase.value_function:
        def v_evaluate_${loop.index}(value):
            <%include file="statement.mako" args="statement=testcase.value_function"/>
    % endif

    def e_evaluate_${loop.index}(value):
        <%include file="statement.mako" args="statement=testcase.exception_function"/>
% endfor


## Run the "before" code if it exists.
${before}


## Prepare the command line arguments if needed.
% if context_testcase.exists and context_testcase.arguments:
    new_args = [sys.argv[0]]
    new_args.extend([\
        % for argument in context_testcase.arguments:
            "${argument}", \
        % endfor
    ])
    sys.argv = new_args
% endif


## Import the code for the first time, which will run the code.
try:
    from ${submission_name} import *
except Exception as e:
    ## If there is a main test case, pass the exception to it.
    % if context_testcase.exists:
        e_evaluate_main(e)
    % else:
        raise e
    % endif
% if context_testcase.exists:
    else:
        e_evaluate_main(None)
% endif

write_delimiter("--${secret_id}-- SEP")

## Generate the actual tests based on the context.
% for testcase in testcases:
    <% testcase: _TestcaseArguments %>
    try:
        ## If we have a value function, we have an expression.
        % if testcase.value_function:
            v_evaluate_${loop.index}(\
        % endif
        <%include file="statement.mako" args="statement=testcase.command" />\
        % if testcase.value_function:
            )\
        % endif

    except Exception as e:
        e_evaluate_${loop.index}(e)
    else:
        e_evaluate_${loop.index}(None)

    write_delimiter("--${secret_id}-- SEP")

% endfor

${after}

## Close output files.
value_file.close()
exception_file.close()
