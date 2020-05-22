## This translates a function call to Python.
<%! from tested.serialisation import FunctionType, NamedArgument, Expression %>
<%! from tested.utils import get_args %>
<%page args="function" />
% if function.type == FunctionType.NAMESPACE:
    ${function.namespace}.\
% endif
${function.name}\
% if function.type != FunctionType.PROPERTY:
    (\
    % for argument in function.arguments:
        % if isinstance(argument, NamedArgument):
            ${argument.name}=<%include file="statement.mako" args="statement=argument.value"/>
        % else:
            <% assert isinstance(argument, get_args(Expression)) %>
            <%include file="statement.mako" args="statement=argument"/>
        % endif
        % if not loop.last:
            , \
        % endif
    % endfor
    )\
% endif