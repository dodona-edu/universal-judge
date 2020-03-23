## This generates a function expression in Java.
<%! from tested.serialisation import FunctionType %>
<%page args="function" />
% if function.type == FunctionType.CONSTRUCTOR:
    new \
% endif
% if function.type == FunctionType.NAMESPACE or (function.type == FunctionType.FUNCTION and function.namespace):
    ${function.namespace}.\
% endif
${function.name}\
% if function.type != FunctionType.PROPERTY:
    (\
    % for argument in function.arguments:
        <%include file="expression.mako" args="expression=argument"/>
        % if not loop.last:
            , \
        % endif
    % endfor
    )\
% endif
