## This translates a function call to JavaScript.
<%! from tested.serialisation import FunctionType %>\
<%page args="function" />\
% if function.type == FunctionType.NAMESPACE or (function.type == FunctionType.FUNCTION and function.namespace):
    ${function.namespace}.\
% endif
%if function.type == FunctionType.CONSTRUCTOR:
new \
    %if function.namespace:
${function.namespace}.\
    %endif
%endif
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