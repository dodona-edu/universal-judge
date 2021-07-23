## Convert a statement and/or expression into JavaScript code.
<%! from tested.utils import get_args %>\
<%! from tested.serialisation import Value, Identifier, FunctionCall, Assignment, Namespace %>\
<%page args="statement,internal=False"/>\
% if isinstance(statement, Identifier):
    ## Statement is an identifier => just echo it.
    ${statement}\
% elif isinstance(statement, FunctionCall):
    ## Statement is a function call => delegate to function template.
    <%include file="function.mako" args="function=statement,internal=internal"/>\
% elif isinstance(statement, get_args(Value)):
    ## Statement is a literal => delegate to the value template.
    <%include file="value.mako", args="value=statement" />\
% elif isinstance(statement, Namespace):
    ${"_".join(statement.package_path + [statement.name])}\
% elif isinstance(statement, get_args(Assignment)):
    ## Statement is an assignment => recursively delegate right-hand side to statement template.
    % if full:
        ## TODO: We should make a distinction between variable and constant declarations
        let \
    % endif
    ${statement.variable} = <%include file="statement.mako" args="statement=statement.expression,internal=True"/>\
% else:
    <% assert False, "invalid statement" %>\
% endif