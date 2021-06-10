## This generates a function expression in C.
<%! from tested.testplan import FunctionType %>\
<%page args="function" />\
${function.name}\
% if function.type == FunctionType.FUNCTION:
    (\
    % for argument in function.arguments:
        <%include file="statement.mako" args="statement=argument"/>\
        % if not loop.last:
            , \
        % endif
    % endfor
    )\
% endif