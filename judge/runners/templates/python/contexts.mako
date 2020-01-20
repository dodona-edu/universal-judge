## Code to execute one test context.
<%! from testplan import Assignment %>
import sys
import evaluator

## Get which context we are currently testing.
number = int(sys.argv[1])

## Depending on the context, there may be some arguments
% for c in contexts:
    % if c.main_testcase.exists and c.main_testcase.arguments:
        if ${loop.index} == number:
            sys.argv.extend([\
            % for argument in c.main_testcase.arguments:
                <%include file="value.mako" args="value=argument"/>\
            % endfor
            ])
    % endif
% endfor

## Open the output files.
evaluator.open_outputs()

## Depending on the context, there might be a "before".
% for c in contexts:
    % if c.main_testcase.exists and c.before:
        if ${loop.index} == number:
            ${c.before}
    % endif
% endfor

## Import the code for the first time.
try:
    from ${c.submission_name} import *
except Exception as e:
    evaluator.e_evaluate_main(e)
    % for c in contexts:
        % if c.main_testcase.exists:
            if ${loop.index} == number:
                sys.stderr.write("--${secret_id}-- SEP")
                sys.stdout.write("--${secret_id}-- SEP")
                evaluator.write_delimiter("--${secret_id}-- SEP")
        % endif
    % endfor


## Generate the actual tests based on the context.
% for c in contexts:
    if ${loop.index} == number:
        ## Handle test cases.
        % for additional in c.additional_testcases:
            try:
                % if isinstance(additional.statement, Assignment):
                    <%include file="assignment.mako" args="assignment=additional.statement" />\
                % else:
                    % if additional.has_return:
                        evaluator.v_evaluate_${loop.index}(\
                    % endif
                    <%include file="function.mako" args="function=additional.statement" />\
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
% endfor

## Depending on the context, there might be an "after".
% for c in contexts:
    % if c.main_testcase.exists and c.after:
        if ${loop.index} == number:
            ${c.after}
    % endif
% endfor

## Close output files.
evaluator.close_outputs()
