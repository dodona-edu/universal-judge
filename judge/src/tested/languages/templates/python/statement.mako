## Generates a statement to code in Python.
<%page args="statement"/>
${statement.name} = <%include file="expression.mako" args="function=statement.expression"/>
