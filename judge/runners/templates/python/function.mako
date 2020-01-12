## This generates a function call in Python.
<%! from testplan import FunctionType %>
<%page args="function" />
% if function.type == FunctionType.CONSTRUCTOR:
    <%include file="value.mako" args="value=function.object"/>
% else:
    % if function.type == FunctionType.TOP:
        ${function.name}\
    % elif function.type == FunctionType.OBJECT:
        ${function.object}.${function.name}\
    % endif
% endif
% if function.type != FunctionType.IDENTITY:
    (\
% endif
% for argument in function.arguments:
    <%include file="value.mako" args="value=argument"/>
    % if not loop.last:
        , \
    % endif
% endfor
% if function.type != FunctionType.IDENTITY:
    )\
% endif