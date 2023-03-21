## This generates a function expression in Haskell.
<%! from tested.serialisation import FunctionType, Value %>\
<%! from tested.utils import get_args %>\
<%page args="function" />\
% if function.namespace:
    <%include file="statement.mako" args="statement=function.namespace"/>.\
% endif
${function.name} \
% for argument in function.arguments:
    % if isinstance(argument, get_args(Value)):
        <%include file="statement.mako" args="statement=argument"/>\
    % else:
        (<%include file="statement.mako" args="statement=argument"/>)\
    % endif
    % if not loop.last:
         \
    % endif
% endfor