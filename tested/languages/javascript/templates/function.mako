## This translates a function call to JavaScript.
<%! from tested.serialisation import FunctionType %>\
await \
<%page args="function" />\
% if function.type == FunctionType.CONSTRUCTOR:
    new \
% endif
% if function.namespace:
    ${function.namespace}.\
% endif
${function.name}\
% if function.type != FunctionType.PROPERTY:
    (\
    % for argument in function.arguments:
        <%include file="statement.mako" args="statement=argument"/>\
        % if not loop.last:
            , \
        % endif
    % endfor
    )\
% endif