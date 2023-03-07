## Code to execute one context.
<%! from tested.languages.generator import _TestcaseArguments, MainCallArguments %>\
<%! from tested.serialisation import Statement, Expression %>\
<%! from tested.utils import get_args %>\
import values
import sys
import importlib
from decimal import Decimal

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

def write_context_separator():
    value_file.write("--${context_secret_id}-- SEP")
    exception_file.write("--${context_secret_id}-- SEP")
    sys.stderr.write("--${context_secret_id}-- SEP")
    sys.stdout.write("--${context_secret_id}-- SEP")
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
def send_specific_value(value):
    values.send_evaluated(value_file, value)

## Send the result of a language specific exception evaluator to TESTed.
def send_specific_exception(exception):
    values.send_evaluated(exception_file, exception)

## Generate the functions that will execute for each context.
% for i, ctx in enumerate(contexts):
    def ${execution_name}_context_${i}():
        ${ctx.before}

        ## Import the code if there is no main testcase.
        % if not ctx.context.has_main_testcase():
            import ${submission_name}
            % if i != 0:
                importlib.reload(sys.modules["${submission_name}"])
            % endif
        % endif

        % for tc in ctx.testcases:
            write_separator()

            ## Prepare the command line arguments if needed.
            % if tc.testcase.is_main_testcase():
                new_args = [sys.argv[0]]
                new_args.extend([\
                    % for argument in tc.input.arguments:
                        "${argument}", \
                    % endfor
                ])
                sys.argv = new_args
            % endif

            try:
                % if tc.testcase.is_main_testcase():
                    ## If it is a main tc, import the code, which will call the main function.
                    import ${submission_name}
                    % if i != 0:
                        importlib.reload(sys.modules["${submission_name}"])
                    % endif
                % else:
                    ## If we have a value function, we have an expression.
                    <% stmt = tc.input.input_statement() %>
                    <%include file="statement.mako" args="statement=stmt,with_namespace=True" />
                % endif
            except Exception as e:
                <%include file="statement.mako" args="statement=tc.exception_statement('e')" />
            else:
                <%include file="statement.mako" args="statement=tc.exception_statement()" />
        % endfor
        ${ctx.after}
% endfor

% for i, ctx in enumerate(contexts):
    write_context_separator()
    ${execution_name}_context_${i}()
% endfor

## Close output files.
value_file.close()
exception_file.close()
