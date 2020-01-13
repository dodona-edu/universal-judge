## Generates an assignment in Java. Since we use Java11+, we can use var.
<%page args="assignment"/>
var ${assignment.name} = <%include file="function.mako" args="function=assignment.expression"/>;