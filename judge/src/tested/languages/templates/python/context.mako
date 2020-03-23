## Code to execute_module one test context.
<%! from tested.serialisation import Statement, Expression %>
<%! from typing import get_args %>
import values
import sys

## Import the language specific evaluators we will need.
% for name in evaluator_names:
    import ${name}
% endfor


## Prepare some code for the evaluation.
value_file = open(r"${value_file}", "w")
exception_file = open(r"${exception_file}", "w")


def write_delimiter(delimiter):
    value_file.write(delimiter)
    exception_file.write(delimiter)


def send(value):
    values.send_value(value_file, value)


def send_exception(exception):
    values.send_exception(exception_file, exception)

def e_evaluate_main(value):
    <%include file="expression.mako" args="expression=context_testcase.exception_function"/>

% for additional in testcases:
    % if additional.has_return:
        def v_evaluate_${loop.index}(value):
            <%include file="expression.mako" args="expression=additional.value_function"/>
    % endif

    def e_evaluate_${loop.index}(value):
        <%include file="expression.mako" args="expression=additional.exception_function"/>
% endfor


## Prepare arguments for context_testcase testcase if needed.
% if context_testcase.exists and context_testcase.arguments:
    sys.argv.extend([\
        % for argument in context_testcase.arguments:
            "${argument}", \
        % endfor
    ])
% endif

## Run the "before" code.
% if before:
    ${before}
% endif


## Import the code for the first time.
try:
    from ${submission_name} import *
except Exception as e:
    % if context_testcase.exists:
        e_evaluate_main(e)
    % else:
        raise e
    % endif

sys.stderr.write("--${secret_id}-- SEP")
sys.stdout.write("--${secret_id}-- SEP")
write_delimiter("--${secret_id}-- SEP")

## Generate the actual tests based on the context.
% for additional in testcases:
    try:
        % if isinstance(additional.command, get_args(Statement)):
            <%include file="statement.mako" args="statement=additional.command" />\
        % else:
            <% assert isinstance(additional.command, get_args(Expression)) %>
            % if additional.has_return:
                v_evaluate_${loop.index}(\
            % endif
            <%include file="expression.mako" args="expression=additional.command" />\
            % if additional.has_return:
                )\
            % endif
        % endif

    except Exception as e:
        e_evaluate_${loop.index}(e)

    sys.stderr.write("--${secret_id}-- SEP")
    sys.stdout.write("--${secret_id}-- SEP")
    write_delimiter("--${secret_id}-- SEP")

% endfor

% if after:
    ${after}
% endif

## Close output files.
value_file.close()
exception_file.close()
