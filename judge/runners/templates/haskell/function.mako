<%! from testplan import FunctionType %>
<%page args="function" />
## This generates a function call in Python.
% if function.type == FunctionType.TOP and has_top_level:
    ${function.name}\
% elif function.type == FunctionType.STATIC or function.type == FunctionType.INSTANCE or (function.type == FunctionType.TOP and not has_top_level):
    ${function.object}.${function.name}
% elif function.type == FunctionType.MAIN:
    main\
% endif
 \
% for argument in function.arguments:
    <%include file="argument.mako" args="argument=argument"/>
    % if not loop.last:
         \
    % endif
% endfor
