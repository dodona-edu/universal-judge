## Code to execute one test context.
import sys
import values
import evaluator_${context_id}

## Set the main arguments, if needed.
% if execution.exists:
    sys.argv.extend([\
        % for argument in execution.arguments:
            <%include file="value.mako" args="value=argument"/>\
        % endfor
    ])
% endif

## Open the output files.
evaluator_${context_id}.open_outputs()

## Import the code for the first time.
try:
    import ${submission_name}
except Exception as e:
    evaluator_${context_id}.e_evaluate_execution(e)


## Handle test cases.
% for additional in additionals:
    sys.stderr.write("--${secret_id}-- SEP")
    sys.stdout.write("--${secret_id}-- SEP")
    evaluator_${context_id}.write_delimiter("--${secret_id}-- SEP")
    try:
        % if additional.has_return:
            evaluator_${context_id}.v_evaluate_${context_id}_${loop.index}(${submission_name}.<%include file="function.mako" args="function=additional.function" />)
        % else:
            ${submission_name}.<%include file="function.mako" args="function=additional.function" />;
        % endif
    except Exception as e:
        evaluator_${context_id}.e_evaluate_${context_id}_${loop.index}(e)
% endfor

## Close output files.
evaluator_${context_id}.close_outputs()
