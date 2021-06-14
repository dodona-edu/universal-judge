<%page args="tp" />\
<% params = len(tp.parameter_types) %>\
% if params > 2:
    <% raise ValueError("Java does only support maximum two parameters for the functional interfaces in the 'java.util.function' package") %>\
% elif params == 0 and tp.return_type is None:
    Runnable\
% elif params == 0:
    Supplier<<%include file="declaration.mako" args="tp=tp.return_type,value=None,inner=True"/>>\
% elif tp.return_type is None:
    % if params == 1:
        Consumer<<%include file="declaration.mako" args="tp=tp.parameter_types[0],value=None,inner=True"/>>\
    % else:
        BiConsumer<<%include file="declaration.mako" args="tp=tp.parameter_types[0],value=None,inner=True"/>, \
        <%include file="declaration.mako" args="tp=tp.parameter_types[1],value=None,inner=False"/>>\
    % endif
% elif tp.is_operator():
    % if params == 1:
        UnaryOperator<<%include file="declaration.mako" args="tp=tp.return_type,value=None,inner=True"/>>\
    % else:
        BinaryOperator<<%include file="declaration.mako" args="tp=tp.return_type,value=None,inner=True"/>>\
    % endif
% else:
    % if params == 1:
        Function<<%include file="declaration.mako" args="tp=tp.parameter_types[0],value=None,inner=True"/>, \
        <%include file="declaration.mako" args="tp=tp.return_type,value=None,inner=True"/>>\
    % else:
        BiFunction<<%include file="declaration.mako" args="tp=tp.parameter_types[0],value=None,inner=True"/>, \
        <%include file="declaration.mako" args="tp=tp.parameter_types[1],value=None,inner=True"/>, \
        <%include file="declaration.mako" args="tp=tp.return_type,value=None,inner=True"/>>\
    % endif
% endif