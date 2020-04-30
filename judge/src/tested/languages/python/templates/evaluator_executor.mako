## Responsible for generating a function expression to a custom evaluator.
<%page args="evaluator,function" />

import ${evaluator}
import values
import sys

result = <%include file="function.mako" args="function=function" />
values.send_evaluated(sys.stdout, result)
