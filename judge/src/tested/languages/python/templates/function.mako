## This translates a function call to Python.
<%! from tested.serialisation import FunctionType%>
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
