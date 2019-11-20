## This generates a function call in Python.
<%! from testplan import FunctionType %>
<%page args="function" />
% if function.type == FunctionType.TOP:
    ${function.name}\
% elif function.type == FunctionType.STATIC or function.type == FunctionType.INSTANCE:
    ${function.object}.${function.name}\
% endif
(\
% for argument in function.arguments:
    <%include file="value.mako" args="value=argument"/>
    % if not loop.last:
        , \
    % endif
% endfor
)