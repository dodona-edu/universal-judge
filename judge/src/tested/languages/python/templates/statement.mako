## Generates a command to code in Python.
<%page args="statement"/>
${statement.name} = <%include file="expression.mako" args="expression=statement.expression"/>
