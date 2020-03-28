## Generates an statement in Java.
<%page args="statement,full=False"/>
% if full:
    <%include file="declaration.mako" args="tp=statement.type" /> \
% endif
${statement.name} = <%include file="expression.mako" args="expression=statement.expression"/>;
