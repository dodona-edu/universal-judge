## This generates a function expression in Java.
<%! from tested.serialisation import FunctionType %>\
<%page args="function" />\
% if function.type == FunctionType.CONSTRUCTOR:
    new \
% endif
% if function.namespace and not (function.has_root_namespace and function.type in (FunctionType.CONSTRUCTOR, FunctionType.CONSTRUCTOR_REFERENCE)):
    <%include file="statement.mako" args="statement=function.namespace"/>\
    % if function.type != FunctionType.FUNCTION_REFERENCE:
        .\
    % endif
% endif
% if function.type == FunctionType.CONSTRUCTOR_REFERENCE:
    ${function.name}::new\
% elif function.type == FunctionType.FUNCTION_REFERENCE:
    ::${function.name}\
% else:
    ${function.name}\
% endif
% if function.type in (FunctionType.CONSTRUCTOR, FunctionType.FUNCTION):
    (\
    % for argument in function.arguments:
        <%include file="statement.mako" args="statement=argument"/>\
        % if not loop.last:
            , \
        % endif
    % endfor
    )\
% endif