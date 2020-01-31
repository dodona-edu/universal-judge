## Responsible for generating a function call to a custom evaluator.
<%page args="evaluator,expected,actual,arguments" />

import ${evaluator}

${evaluator}.evaluate(
    expected=<%include file="value.mako" args="value=expected"/>,
    actual=<%include file="value.mako" args="value=actual"/>,
    arguments=<%include file="value.mako" args="value=arguments"/>
)
