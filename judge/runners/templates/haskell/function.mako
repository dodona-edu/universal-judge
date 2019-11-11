<%! from testplan import FunctionType %>
<%page args="function" />
## This generates a function call in Python.
% if function.type == FunctionType.top and has_top_level:
    ${function.name}\
% elif function.type == FunctionType.static or function.type == FunctionType.instance or (function.type == FunctionType.top and not has_top_level):
    ${function.object}.${function.name}
% elif function.type == FunctionType.main:
    main\
% endif
\
% for argument in function.arguments:
    <%include file="argument.mako" args="argument=argument"/>
    % if not loop.last:
         \
    % endif
% endfor
