## Code to execute one test context.
<%! from testplan import Assignment %>
import sys
import evaluators

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
evaluators.open_outputs()

## Depending on the context, there might be a "before".
% for c in contexts:
    % if c.main_testcase.exists and c.before:
        if ${loop.index} == number:
            ${c.before}
    % endif
% endfor

## Import the code for the first time.
try:
    from ${submission_name} import *
except Exception as e:
    % for c in contexts:
        % if c.main_testcase.exists:
            if ${loop.index} == number:
                evaluators.e_evaluate_main_${loop.index}(e)
        % endif
    % endfor
    pass

<% with_main = (i for i,c in enumerate(contexts) if c.main_testcase.exists) %>
if number in ${tuple(with_main)}:
    sys.stderr.write("--${secret_id}-- SEP")
    sys.stdout.write("--${secret_id}-- SEP")
    evaluators.write_delimiter("--${secret_id}-- SEP")

## Generate the actual tests based on the context.
% for c in contexts:
    <% c_number = loop.index %>
    % if c.additional_testcases:
        if ${c_number} == number:
            ## Handle test cases.
            % for additional in c.additional_testcases:
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
    % endif
% endfor

## Depending on the context, there might be an "after".
% for c in contexts:
    % if c.main_testcase.exists and c.after:
        if ${loop.index} == number:
            ${c.after}
    % endif
% endfor

## Close output files.
evaluators.close_outputs()
