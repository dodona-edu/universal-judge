<%! from testplan import FunctionType %>
<%page args="function" />
## This generates a function call in Haskell.
% if function.type == FunctionType.TOP:
    ${function.name}\
% elif function.type == FunctionType.OBJECT:
    ${function.object}.${function.name}
% endif
 \
% for argument in function.arguments:
    <%include file="value.mako" args="value=argument"/>
    % if not loop.last:
         \
    % endif
% endfor
