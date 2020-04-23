## This generates a function expression in Java.
<%! from tested.testplan import FunctionType %>
<%page args="function" />
% if function.type == FunctionType.NAMESPACE:
    ${function.namespace}.\
% endif
${function.name}\
% if function.type != FunctionType.PROPERTY:
    (\
    % for argument in function.arguments:
        <%include file="statement.mako" args="statement=argument"/>
        % if not loop.last:
            , \
        % endif
    % endfor
    )\
% endif
