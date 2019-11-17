<%! from testplan import FunctionType %>
<%page args="function" />
## This generates a function call in Java.
% if function.type == FunctionType.STATIC or function.type == FunctionType.INSTANCE or function.type == FunctionType.TOP:
    % if function.object:
        ${function.object}.\
    % endif
    ${function.name}\
% elif function.type == FunctionType.MAIN:
    ${function.object}.main\
% endif
(\
% if function.type == FunctionType.MAIN:
    new String[]{\
% endif
% for argument in function.arguments:
    <%include file="argument.mako" args="argument=argument"/>
    % if not loop.last:
        , \
    % endif
% endfor
% if function.type == FunctionType.MAIN:
    }\
% endif
)
