<%! from testplan import FunctionType %>
<%page args="function" />
## This generates a function call in Java.
${function.object}.${function.name}\
(\
% for argument in function.arguments:
    <%include file="value.mako" args="value=argument"/>
    % if not loop.last:
        , \
    % endif
% endfor
)
