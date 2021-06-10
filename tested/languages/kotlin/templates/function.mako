## This translates a function call to Kotlin.
<%! from tested.serialisation import FunctionType, NamedArgument, Expression %>\
<%! from tested.utils import get_args %>\
<%page args="function" />\
% if not function.has_root_namespace and function.namespace:
    ## Kotlin has the !! (non-value assertion) operator
    <%include file="statement.mako" args="statement=function.namespace"/>!!\
    % if function.type not in (FunctionType.FUNCTION_REFERENCE, FunctionType.CONSTRUCTOR_REFERENCE):
        .\
    % endif
% endif
% if function.type in (FunctionType.CONSTRUCTOR_REFERENCE, FunctionType.FUNCTION_REFERENCE):
    ::${function.name}\
% else:
    ${function.name}\
% endif
% if function.type in (FunctionType.CONSTRUCTOR, FunctionType.FUNCTION):
    (\
    % for argument in function.arguments:
        % if isinstance(argument, NamedArgument):
            ${argument.name}=<%include file="statement.mako" args="statement=argument.value"/>\
        % else:
            <% assert isinstance(argument, get_args(Expression)) %>\
            <%include file="statement.mako" args="statement=argument"/>\
        % endif
        % if not loop.last:
            , \
        % endif
    % endfor
    )\
% endif