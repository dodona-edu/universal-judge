## Generates an convert_statement in Haskell.
<%page args="assignment,root=True"/>\
% if root:
    ${assignment.variable} <- <%include file="function.mako" args="function=assignment.expression"/>\
% else:
    let ${assignment.variable} = <%include file="function.mako" args="function=assignment.expression,lifting=False"/>\
% endif