## Generates an statement in Java.
<%page args="statement,root=False"/>
% if root:
    ${statement.name} <- <%include file="expression.mako" args="expression=statement.expression, lifting=True"/>
% else:
    let ${statement.name} = <%include file="expression.mako" args="expression=statement.expression"/>
% endif
