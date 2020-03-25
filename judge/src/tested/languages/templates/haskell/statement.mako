## Generates an statement in Java.
<%page args="statement,root=False"/>
% if root:
    ${assignment.name} <- <%include file="expression.mako" args="expression=statement.expression"/>
% else:
    let ${assignment.name} = <%include file="expression.mako" args="expression=statement.expression,lifting=False"/>
% endif
