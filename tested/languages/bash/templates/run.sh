<%! from json import dumps %>\
function write_context_separator {
    echo -n "--${context_secret_id}-- SEP" >>${value_file}
    echo -n "--${context_secret_id}-- SEP" >>${exception_file}
    echo -n "--${context_secret_id}-- SEP"
    echo -n "--${context_secret_id}-- SEP" >&2
}

function write_separator {
    echo -n "--${secret_id}-- SEP" >>${value_file}
    echo -n "--${secret_id}-- SEP" >>${exception_file}
    echo -n "--${secret_id}-- SEP"
    echo -n "--${secret_id}-- SEP" >&2
}

function json_escape {
    printf '%s' "$1" | python -c 'import json,sys; print(json.dumps(sys.stdin.read()))'
}

function send_value {
    echo -n "{\"type\": \"text\", \"data\": $(json_escape "$1")}" >>${value_file}
}

touch ${value_file} ${exception_file}

% for i, ctx in enumerate(contexts):
    function context_${i} {
        ${ctx.before}

        ## Import the code if there is no main testcase.
        % if not ctx.context.has_main_testcase():
            source ./${submission_name}.sh
        % endif

        % for tc in ctx.testcases:
            write_separator

            % if tc.testcase.is_main_testcase():
                ## If it is a main tc, import the code, which will call the main function.
                source ./${submission_name}.sh \
                % for argument in tc.input.arguments:
                    "${argument.replace("\\", "\\\\").replace('"', '\\"')}" \
                % endfor
            % else:
                ## If we have a value function, we have an expression.
                <%include file="statement.mako" args="statement=tc.input.input_statement()" />
            % endif
        % endfor
        ${ctx.after}
    }
% endfor

% for i, ctx in enumerate(contexts):
    write_context_separator
    context_${i}
% endfor

exit $?
