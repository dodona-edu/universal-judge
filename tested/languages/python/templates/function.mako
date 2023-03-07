## This translates a function call to Python.
<%! from tested.serialisation import FunctionType, NamedArgument, Expression %>\
<%! from tested.utils import get_args %>\
<%page args="function,with_namespace=False" />\
% if function.namespace and (not function.has_root_namespace or with_namespace):
    <%include file="statement.mako" args="statement=function.namespace,with_namespace=with_namespace"/>.\
% endif
${function.name}\
% if function.type != FunctionType.PROPERTY:
    (\
    % for argument in function.arguments:
        % if isinstance(argument, NamedArgument):
            ${argument.name}=<%include file="statement.mako" args="statement=argument.value,with_namespace=with_namespace"/>\
        % else:
            <% assert isinstance(argument, get_args(Expression)) %>\
            <%include file="statement.mako" args="statement=argument,with_namespace=with_namespace"/>\
        % endif
        % if not loop.last:
            , \
        % endif
    % endfor
    )\
% endif
