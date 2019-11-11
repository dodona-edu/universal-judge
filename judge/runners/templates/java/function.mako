<%! from testplan import FunctionType %>
<%page args="function" />
## This generates a function call in Java.
% if function.type == FunctionType.static or function.type == FunctionType.instance or function.type == FunctionType.top:
    % if function.object:
        ${function.object}.\
    % endif
    ${function.name}\
% elif function.type == FunctionType.main:
    ${function.object}.main\
% endif
(\
% if function.type == FunctionType.main:
    new String[]{\
% endif
% for argument in function.arguments:
    <%include file="argument.mako" args="argument=argument"/>
    % if not loop.last:
        , \
    % endif
% endfor
% if function.type == FunctionType.main:
    }\
% endif
)
