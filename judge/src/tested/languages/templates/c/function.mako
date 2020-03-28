## This generates a function expression in Java.
<%! from testplan import FunctionType %>
<%page args="function" />
% if function.type != FunctionType.IDENTITY:
    % if function.type == FunctionType.NAMESPACE or (function.type == FunctionType.FUNCTION and function.namespace):
        ${function.namespace}.\
    % endif
    ${function.name}\
    (\
% endif
% for argument in function.arguments:
    <%include file="value.mako" args="value=argument"/>
    % if not loop.last:
        , \
    % endif
% endfor
% if function.type != FunctionType.IDENTITY:
    )\
% endif
