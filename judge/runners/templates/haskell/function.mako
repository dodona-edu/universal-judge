## This generates a function call in Haskell.
<%! from testplan import FunctionType %>
<%page args="function,lifting=True" />
% if function.type != FunctionType.IDENTITY:
    % if function.type == FunctionType.NAMESPACE or (function.type == FunctionType.FUNCTION and function.namespace):
        ${function.namespace}.\
    % endif
    ${function.name} \
% elif lifting:
    return \
% endif
% for argument in function.arguments:
    <%include file="literal.mako" args="value=argument"/>
    % if not loop.last:
         \
    % endif
% endfor
