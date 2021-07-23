## Translate "statements" and expressions to Haskell.
<%! from tested.utils import get_args %>\
<%! from tested.serialisation import Value, Identifier, FunctionCall, Expression, Assignment, Namespace %>\
<%page args="statement,root=False,lifting=False"/>\
% if isinstance(statement, get_args(Expression)):
    % if lifting:
        return (\
    % endif
    % if isinstance(statement, Identifier):
        ## If the expression is an identifier, just echo it.
    ${statement}\
    % elif isinstance(statement, FunctionCall):
        ## Delegate to the function template for function calls.
    <%include file="function.mako" args="function=statement"/>\
    % elif isinstance(statement, Namespace):
        ## Namespace
    ${".".join(statement.package_path + [statement.name])}\
    % else:
        <% assert isinstance(statement, get_args(Value)) %>\
        ## We have a value, delegate to the value template.
    <%include file="value.mako", args="value=statement" />\
    % endif
    % if lifting:
        )\
    % endif
% else:
    <% assert isinstance(statement, get_args(Assignment)) %>\
    % if root:
        ${statement.variable} <- <%include file="statement.mako" args="statement=statement.expression,lifting=True"/>\
    % else:
        let ${statement.variable} = <%include file="statement.mako" args="statement=statement.expression"/>\
    % endif
% endif