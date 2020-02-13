## Code to execute one test context.
<%! from testplan import Assignment %>
import values


## Prepare some code for the evaluation.
value_file = open(r"${value_file}", "w")
exception_file = open(r"${exception_file}", "w")

def write_delimiter(delimiter):
    value_file.write(delimiter)
    exception_file.write(delimiter)


def evaluated(result, expected, actual, messages=[]):
    values.send_evaluated(value_file, result, expected, actual, messages)


def send(value):
    values.send_value(value_file, value)


def send_exception(exception):
    values.send_exception(exception_file, exception)

${main_testcase.exception_code}

% for additional in additional_testcases:
    % if additional.has_return:
        ${additional.value_code}
    % endif

    ${additional.exception_code}
% endfor


## Prepare arguments for main testcase if needed.
% if main_testcase.exists and main_testcase.arguments:
    sys.argv.extend([\
        % for argument in main_testcase.arguments:
            <%include file="value.mako" args="value=argument"/>\
        % endfor
    ])
% endif

## Run the "before" code for the main testcase if needed.
% if main_testcase.exists and before:
    ${c.before}
% endif


## Import the code for the first time.
try:
    from ${submission_name} import *
except Exception as e:
    % if c.main_testcase.exists:
        evaluators.e_evaluate_main(e)
    % else:
        raise e
    % endif


% if main_testcase.exists:
    sys.stderr.write("--${secret_id}-- SEP")
    sys.stdout.write("--${secret_id}-- SEP")
    evaluators.write_delimiter("--${secret_id}-- SEP")

## Generate the actual tests based on the context.
% for additional in additional_testcases:
    try:
        % if isinstance(additional.statement, Assignment):
            <%include file="assignment.mako" args="assignment=additional.statement" />\
        % else:
            % if additional.has_return:
                evaluators.v_evaluate_${c_number}_${loop.index}(\
            % endif
            <%include file="function.mako" args="function=additional.statement" />\
            % if additional.has_return:
                )\
            % endif
        % endif

    except Exception as e:
        evaluators.e_evaluate_${c_number}_${loop.index}(e)

    sys.stderr.write("--${secret_id}-- SEP")
    sys.stdout.write("--${secret_id}-- SEP")
    evaluators.write_delimiter("--${secret_id}-- SEP")

% endfor

## Run the "after" code for the main testcase if needed.
% if main_testcase.exists and before:
    ${c.before}
% endif

## Close output files.
value_file.close()
exception_file.close()
