<%! from json import dumps %>
function json_escape {
    printf '%s' "$1" | python -c 'import json,sys; print(json.dumps(sys.stdin.read()))'
}

function send_value {
    echo "{\"type\": \"text\", \"data\": $(json_escape "$1")}"
}
## Convert a statement and/or expression into BASH code.
<%! from tested.utils import get_args %>\
<%! from tested.serialisation import FunctionCall, FunctionType %>\

% for statement in statements:
    <% stmt = FunctionCall(type=FunctionType.FUNCTION, name='send_value', arguments=[statement]) %>\
    <%include file="statement.mako" args="statement=stmt"/>
% endfor
