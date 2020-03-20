## Responsible for generating a function expression to a custom evaluator.
<%page args="evaluator,expected,actual,arguments" />

import ${evaluator}

${evaluator}.evaluate(
    expected=<%include file="literal.mako" args="value=expected"/>,
    actual=<%include file="literal.mako" args="value=actual"/>,
    arguments=<%include file="literal.mako" args="value=arguments"/>
)
