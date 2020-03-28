## This translates an expression to Java.
<%! from tested.utils import get_args %>
<%! from tested.serialisation import Value, Identifier, FunctionCall%>
<%page args="expression" />
% if isinstance(expression, Identifier):
    ## If the expression is an identifier, just echo it.
    ${expression}\
% elif isinstance(expression, FunctionCall):
    ## Delegate to the function template for function calls.
    <%include file="function.mako" args="function=expression"/>
% else:
    <% assert isinstance(expression, get_args(Value))%>
    ## We have a value, delegate to the value template.
    <%include file="value.mako", args="value=expression" />
% endif
