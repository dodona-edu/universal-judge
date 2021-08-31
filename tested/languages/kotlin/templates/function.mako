## This translates a function call to Kotlin.
<%! from tested.serialisation import FunctionType, NamedArgument, Expression, Namespace %>\
<%! from tested.utils import get_args %>\
<%page args="function" />\
% if not function.has_root_namespace and function.namespace:
    <%include file="statement.mako" args="statement=function.namespace"/>\
    ## Kotlin has the !! (non-value assertion) operator use for identiefiers, values and function calls (possible null)
    % if not isinstance(function.namespace, Namespace):
        !!\
    % endif
    .\
% endif
${function.name}\
% if function.type != FunctionType.PROPERTY:
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