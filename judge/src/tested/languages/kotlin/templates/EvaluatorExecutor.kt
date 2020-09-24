## Responsible for generating a function expression to a custom evaluator.
<%page args="evaluator,function" />\

import java.io.PrintWriter

fun main() {
    val result = <%include file="function.mako" args="function=function" />
    val writer = PrintWriter(System.out)
    sendEvaluated(writer, result)
    writer.flush()
}
