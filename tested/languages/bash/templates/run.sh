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
        % for testcase in ctx.testcases:
            write_separator
            <%include file="statement.mako" args="statement=testcase.input_statement()" />
        % endfor
        ${ctx.after}
    }
% endfor

write_context_separator
source ./${submission_name}.sh \
% for argument in run_testcase.arguments:
    ${dumps(value.data)} \
% endfor


% for i, ctx in enumerate(contexts):
    write_context_separator
    context_${i}
% endfor

exit $?
