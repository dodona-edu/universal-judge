## Convert a statement and/or expression into C code.
<%! from tested.utils import get_args %>
<%! from tested.serialisation import Value, Identifier, FunctionCall, Assignment %>
<%page args="statement,full=False"/>
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
    <% assert isinstance(statement, get_args(Assignment)) %>
    % if full:
        <%include file="declaration.mako" args="value=statement.expression" /> \
    % endif
    ${statement.name} = <%include file="statement.mako" args="statement=statement.expression"/>;
% endif
