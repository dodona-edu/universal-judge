## Code to execute one context.
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
value_file = open("${value_file}", "w")
exception_file = open("${exception_file}", "w")

## Write the separator and flush to ensure the output is in the files.
## This is necessary, otherwise the separators are sometimes missing when
## execution is killed due to timeouts.
def write_separator():
    value_file.write("--${secret_id}-- SEP")
    exception_file.write("--${secret_id}-- SEP")
    sys.stderr.write("--${secret_id}-- SEP")
    sys.stdout.write("--${secret_id}-- SEP")
    sys.stdout.flush()
    sys.stderr.flush()
    value_file.flush()
    exception_file.flush()


##################################
## Predefined functions         ##
##################################

## Send a value to TESTed.
def send_value(value):
    values.send_value(value_file, value)

## Send an exception to TESTed.
def send_exception(exception):
    values.send_exception(exception_file, exception)

## Send the result of a language specific value evaluator to TESTed.
def send_specific_value(r):
    values.send_evaluated(value_file, r)

## Send the result of a language specific exception evaluator to TESTed.
def send_specific_exception(r):
    values.send_evaluated(exception_file, r)

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
    write_separator()
    from ${submission_name} import *
except Exception as e:
    ## If there is a main test case, pass the exception to it.
    % if context_testcase.exists:
        <%include file="statement.mako" args="statement=context_testcase.exception_statement('e')" />
    % else:
        raise e
    % endif
% if context_testcase.exists:
    else:
        <%include file="statement.mako" args="statement=context_testcase.exception_statement()" />
% endif


## Generate the actual tests based on the context.
% for testcase in testcases:
    write_separator()
    <% testcase: _TestcaseArguments %>
    try:
        ## If we have a value function, we have an expression.
        <%include file="statement.mako" args="statement=testcase.input_statement()" />
        
    except Exception as e:
        <%include file="statement.mako" args="statement=testcase.exception_statement('e')" />
    else:
        <%include file="statement.mako" args="statement=testcase.exception_statement()" />

% endfor

${after}

## Close output files.
value_file.close()
exception_file.close()
