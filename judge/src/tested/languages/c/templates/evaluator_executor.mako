## Responsible for generating a function expression to a custom evaluator.
<%page args="evaluator,function" />

#include "values.h"
#include "evaluation_result.h"
#include "${evaluator}.c"


EvaluationResult result = <%include file="function.mako" args="function=function" />
send_evaluated(sys.stdout, result)
