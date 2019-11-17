<%! from testplan import FunctionType %>
<%page args="function" />
## This generates a function call in Python.
% if function.type == FunctionType.TOP:
    ${function.name}\
% elif function.type == FunctionType.STATIC or function.type == FunctionType.INSTANCE:
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
