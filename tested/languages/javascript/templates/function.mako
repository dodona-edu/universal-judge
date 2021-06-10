## This translates a function call to JavaScript.
<%! from tested.serialisation import FunctionType %>\
<%page args="function,internal=False" />\
% if not internal:
    await \
% endif
% if function.type == FunctionType.CONSTRUCTOR:
    new \
% endif
% if function.namespace:
    <%include file="statement.mako" args="statement=function.namespace,internal=True"/>.\
% endif
${function.name}\
% if function.type in (FunctionType.CONSTRUCTOR, FunctionType.FUNCTION):
    (\
    % for argument in function.arguments:
        <%include file="statement.mako" args="statement=argument,internal=True"/>\
        % if not loop.last:
            , \
        % endif
    % endfor
    )\
% endif