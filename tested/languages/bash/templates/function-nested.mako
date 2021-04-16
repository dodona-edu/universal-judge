## This generates a function expression in BASH.
<%! from tested.testplan import FunctionType %>\
<%! from tested.utils import get_args %>\
<%! from tested.serialisation import Value, Identifier, FunctionCall, Assignment %>\
<%!
    def has_nested_function_call(fun):
        return len(list(filter(lambda a: isinstance(a, FunctionCall), fun.arguments)))

        return text.replace("'", "\\'")
%>\
<%page args="function,index,index_fun,index_map" />\
<% index_map.append({}) %>\
% for i, argument in enumerate(function.arguments):
    % if isinstance(argument, FunctionCall):
        ## Delegate to the function template for function calls.
        <% index_map[-1][i] = index_fun() %>\
        % if has_nested_function_call(argument):
            <%include file="function-nested.mako" args="function=argument,index=index_map[-1][i],index_fun=index_fun,index_map=index_map"/>
        % else:
            local ARG${index_map[-1][i]}=$(<%include file="function.mako" args="function=argument,index_fun=index_fun,index_map=index_map"/>)
        % endif
    % endif
% endfor
local ARG${index}=$(${function.name} \
% if function.type != FunctionType.PROPERTY:
    % for i, argument in enumerate(function.arguments):
        % if isinstance(argument, Identifier):
            ## If the expression is an identifier, just echo it.
            "${'$'}${argument}" \
        % elif isinstance(argument, FunctionCall):
            ## Delegate to the function template for function calls.
            "${'$'}ARG${index_map[-1][i]}" \
        % elif isinstance(argument, get_args(Value)):
            ## We have a value, delegate to the value template.
            <%include file="value.mako" args="value=argument" /> \
        % endif
    % endfor
% endif
)\
<% index_map.pop() %>