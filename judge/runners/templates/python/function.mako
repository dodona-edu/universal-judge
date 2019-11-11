<%! from testplan import FunctionType %>
<%page args="function" />
## This generates a function call in Python.
% if function.type == FunctionType.top:
    ${function.name}\
% elif function.type == FunctionType.static or function.type == FunctionType.instance:
    ${function.object}.${function.name}\
% endif
(\
% for argument in function.arguments:
    <%include file="argument.mako" args="argument=argument"/>
    % if not loop.last:
        , \
    % endif
% endfor
)
