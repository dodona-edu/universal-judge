## This generates a function expression in BASH.
<%! from tested.testplan import FunctionType %>\
<%! from tested.utils import get_args %>\
<%! from tested.serialisation import Value, Identifier, FunctionCall, Assignment %>\
<%page args="function" />\
${function.name} \
% if function.type != FunctionType.PROPERTY:
    % for argument in function.arguments:
        % if isinstance(argument, Identifier):
            ## If the expression is an identifier, just echo it.
            "${'$'}${argument}" \
        % elif isinstance(argument, FunctionCall):
            ## Delegate to the function template for function calls.
            $(<%include file="function.mako" args="function=argument"/>) \
        % elif isinstance(argument, get_args(Value)):
            ## We have a value, delegate to the value template.
            <%include file="value.mako" args="value=argument" /> \
        % endif
    % endfor
% endif