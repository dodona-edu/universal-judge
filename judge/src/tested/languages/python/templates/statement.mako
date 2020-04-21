## Convert a statement and/or expression into Python code.
<%! from tested.utils import get_args %>
<%! from tested.serialisation import Value, Identifier, FunctionCall, Assignment %>
<%page args="statement"/>
% if isinstance(statement, Identifier):
    ## If the expression is an identifier, just echo it.
    ${statement}\
% elif isinstance(statement, FunctionCall):
    ## Delegate to the function template for function calls.
    <%include file="function.mako" args="function=statement"/>
% elif isinstance(statement, get_args(Value)):
    ## We have a value, delegate to the value template.
    <%include file="value.mako", args="value=statement" />
% else:
    <% assert isinstance(statement, Assignment)%>
    ## We haven an assignment.
    ${statement.name} = <%include file="statement.mako" args="expression=statement.expression"/>
% endif