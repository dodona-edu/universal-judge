## Responsible for generating a function expression to a custom evaluator.
<%page args="evaluator,function" />
import java.util.*;

public class EvaluatorExecutor {

    public static void main(String[] args) throws Exception {
        var result = <%include file="function.mako" args="function=function" />;
        Values.sendEvaluated(result);
    }
}
