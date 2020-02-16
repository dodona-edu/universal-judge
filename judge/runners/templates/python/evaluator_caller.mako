## Responsible for generating a function call to a custom evaluator.
<%page args="evaluator,value" />
${evaluator}.evaluate(value)
