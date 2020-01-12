## Code to execute one test context.
<%! from testplan import Assignment %>
import sys
import evaluator

## Set the main arguments, if needed.
% if main_testcase.exists:
    sys.argv.extend([\
        % for argument in main_testcase.arguments:
            <%include file="value.mako" args="value=argument"/>\
        % endfor
    ])
% endif

## Open the output files.
evaluator.open_outputs()

${before}

## Import the code for the first time.
try:
    import ${submission_name}
except Exception as e:
    evaluator.e_evaluate_main(e)
% if main_testcase.exists:
    sys.stderr.write("--${secret_id}-- SEP")
    sys.stdout.write("--${secret_id}-- SEP")
    evaluator.write_delimiter("--${secret_id}-- SEP")
% endif


## Handle test cases.
% for additional in additional_testcases:
    try:
        % if isinstance(additional.statement, Assignment):
            <%include file="assignment.mako" args="assignment=additional.statement" />
        % else:
            % if additional.has_return:
                evaluator.v_evaluate_${loop.index}(\
            % endif
            ${submission_name}.<%include file="function.mako" args="function=additional.statement" />\
            % if additional.has_return:
                )\
            % endif
        % endif

    except Exception as e:
        evaluator.e_evaluate_${loop.index}(e)
    sys.stderr.write("--${secret_id}-- SEP")
    sys.stdout.write("--${secret_id}-- SEP")
    evaluator.write_delimiter("--${secret_id}-- SEP")

% endfor

${after}

## Close output files.
evaluator.close_outputs()
