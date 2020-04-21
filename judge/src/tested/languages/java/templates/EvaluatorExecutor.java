## Responsible for generating a function expression to a custom evaluator.
<%page args="evaluator,expected,actual,arguments" />
import java.util.*;

public class EvaluatorExecutor {

    public static void main(String[] args) throws Exception {
        try (AbstractCustomEvaluator eval = new ${evaluator}()) {
            var r = eval.evaluate(
                <%include file="value.mako" args="value=expected"/>,
                <%include file="value.mako" args="value=actual"/>,
                <%include file="value.mako" args="value=arguments"/>
            );
            PrintWriter outFile = new PrintWriter(System.out, true);
            Values.evaluated(outFile, r.result, r.readableExpected, r.readableActual, r.messages);
        }
    }
}
