## This generates a function expression in Haskell.
<%! from testplan import FunctionType %>
<%page args="function,lifting=True" />
% if function.type == FunctionType.NAMESPACE or (function.type == FunctionType.FUNCTION and function.namespace):
    ${function.namespace}.\
% endif
${function.name} \
% for argument in function.arguments:
    <%include file="expression.mako" args="expression=argument"/>
    % if not loop.last:
         \
    % endif
% endfor
