## Generates an assignment in Java. Since we use Java11+, we can use var.
<%page args="assignment, full=False"/>
% if full:
    <%include file="declaration.mako" args="value=assignment.get_type()" />
% endif
${assignment.name} = <%include file="function.mako" args="function=assignment.expression"/>;