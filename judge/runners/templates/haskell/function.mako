## This generates a function call in Haskell.
<%! from testplan import FunctionType %>
<%page args="function,lifting=True" />
% if function.type != FunctionType.IDENTITY:
    % if function.type == FunctionType.OBJECT or function.type == FunctionType.TOP:
        ${function.object}.\
    % endif
    ${function.name} \
% elif lifting:
    return \
% endif
% for argument in function.arguments:
    <%include file="value.mako" args="value=argument"/>
    % if not loop.last:
         \
    % endif
% endfor
