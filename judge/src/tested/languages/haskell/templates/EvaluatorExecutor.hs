<%page args="evaluator,expected,actual,arguments,output" />
module EvaluatorExecutor where

import qualified ${evaluator}
import Values


main = ${evaluator}.evaluate
                "${output}",
                <%include file="value.mako" args="value=expected"/>,
                <%include file="value.mako" args="value=actual"/>,
                <%include file="value.mako" args="value=arguments"/>
