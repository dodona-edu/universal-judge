<%! from testplan import FunctionType %>
<%page args="function" />
## This generates a function call in Java.
% if function.type == FunctionType.OBJECT or function.type == FunctionType.TOP:
    % if function.object:
        ${function.object}.\
    % endif
    ${function.name}\
% endif
(\
% for argument in function.arguments:
    <%include file="argument.mako" args="argument=argument"/>
    % if not loop.last:
        , \
    % endif
% endfor
)
