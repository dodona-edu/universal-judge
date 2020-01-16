## Generates an assignment in Haskell.
<%page args="assignment,root=True"/>
% if root:
    ${assignment.name} <- <%include file="function.mako" args="function=assignment.expression"/>
% else:
    let ${assignment.name} = <%include file="function.mako" args="function=assignment.expression,lifting=False"/>
% endif
