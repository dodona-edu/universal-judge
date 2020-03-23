## Generates an statement in Java.
<%page args="statement,full=False"/>
% if full:
    <%include file="declaration.mako" args="type=statement.type" /> \
% endif
${assignment.name} = <%include file="expression.mako" args="expression=statement.expression"/>;
