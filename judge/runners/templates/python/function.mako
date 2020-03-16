## This generates a function call in Python.
<%! from testplan import FunctionType %>
<%page args="function" />
% if function.type != FunctionType.IDENTITY:
    % if function.type == FunctionType.OBJECT:
        ${function.object}.\
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
