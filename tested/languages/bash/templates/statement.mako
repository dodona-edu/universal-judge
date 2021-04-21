## Convert a statement and/or expression into BASH code.
<%! from tested.utils import get_args %>\
<%! from tested.serialisation import Value, Identifier, FunctionCall, Assignment %>\
<%!
    def unique_index_function():
        index = [-1]
        def get():
            index[0] += 1
            return index[0]

        return get


    index_map = []
%>\
<%page args="statement,full=False"/>\
% if isinstance(statement, Identifier):
    ## If the expression is an identifier, just echo it.
    "${'$'}${statement}"\
% elif isinstance(statement, FunctionCall):
    ## Delegate to the function template for function calls.
    <% index_fun = unique_index_function() %>\
    <%include file="function.mako" args="function=statement,index_fun=index_fun,index_map=index_map"/>\
% elif isinstance(statement, get_args(Value)):
    ## We have a value, delegate to the value template.
    <%include file="value.mako" args="value=statement" />\
% else:
    <% assert isinstance(statement, get_args(Assignment)) %>\
    local ${statement.variable}=\
    % if isinstance(statement.expression, FunctionCall):
        $(<%include file="statement.mako" args="statement=statement.expression"/>)\
    % else:
        <%include file="statement.mako" args="statement=statement.expression"/>\
    % endif
% endif