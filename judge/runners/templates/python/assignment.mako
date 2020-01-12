## Generates an assignment in Python.
<%page args="assignment"/>
${assignment.name} = <%include file="function.mako" args="function=assignment.expression"/>