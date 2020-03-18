## This generates a function call in Java.
<%! from testplan import FunctionType %>
<%page args="function" />
% if function.type == FunctionType.CONSTRUCTOR:
    new
% elif function.type != FunctionType.IDENTITY:
    % if function.type == FunctionType.NAMESPACE or (function.type == FunctionType.FUNCTION and function.namespace):
        ${function.namespace}.\
    % endif
    ${function.name}\
    (\
% endif
% for argument in function.arguments:
    <%include file="literal.mako" args="value=argument"/>
    % if not loop.last:
        , \
    % endif
% endfor
% if function.type != FunctionType.IDENTITY:
    )\
% endif
