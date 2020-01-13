## Generates an assignment in Java. Since we use Java11+, we can use var.
<%page args="assignment"/>
${assignment.name} = <%include file="function.mako" args="function=assignment.expression"/>;