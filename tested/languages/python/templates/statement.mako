## Convert a statement and/or expression into Python code.
<%! from tested.utils import get_args %>\
<%! from tested.serialisation import Value, Identifier, FunctionCall, Assignment, Lambda %>\
<%page args="statement"/>\
% if isinstance(statement, Identifier):
    ## If the expression is an identifier, just echo it.
    ${statement}\
% elif isinstance(statement, FunctionCall):
    ## Delegate to the function template for function calls.
    <%include file="function.mako" args="function=statement"/>\
% elif isinstance(statement, Lambda):
    ## Delegate to the lambda template for lambdas.
    <%include file="lambda.mako" args="lambda_expr=statement"/>\
% elif isinstance(statement, get_args(Value)):
    ## We have a value, delegate to the value template.
    <%include file="value.mako", args="value=statement" />\
% else:
    <% assert isinstance(statement, get_args(Assignment)) %>\
    ## We haven an assignment.
    ${statement.variable} = <%include file="statement.mako" args="statement=statement.expression"/>\
% endif