## Convert a statement and/or expression into code.
<%! from tested.utils import get_args %>
<%! from tested.serialisation import Value, Identifier, FunctionCall, Assignment %>
<%page args="statement,full=False"/>
% if isinstance(statement, Identifier):
    ## If the expression is an identifier, just echo it.
    ${statement}\
% elif isinstance(statement, FunctionCall):
    ## Delegate to the function template for function calls.
    <% raise NotImplementedError("Functions are not implemented yet.") %>
% elif isinstance(statement, get_args(Value)):
    ## We have a value, delegate to the value template.
    <%include file="value.mako" args="value=statement" />
% else:
    <% assert isinstance(statement, get_args(Assignment)) %>
    <% raise NotImplementedError("Assignments are not implemented yet.") %>
% endif
