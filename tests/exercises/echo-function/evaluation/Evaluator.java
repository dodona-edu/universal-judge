import java.util.*;

public class Evaluator {

    public static EvaluationResult evaluate(Object actual) {
        var correct = "correct".equals(actual);
        return EvaluationResult.builder(correct)
                .withReadableExpected("correct")
                .withReadableActual(actual != null ? actual.toString() : "")
                .withMessage(new EvaluationResult.Message("Hallo"))
                .build();
    }

    public static EvaluationResult evaluateValue(Map<String, Object> context) {
        return EvaluationResult.builder(context.get("expected").equals(context.get("actual")))
                .withReadableExpected(context.get("expected").toString())
                .withReadableActual(context.get("actual") != null ? context.get("actual").toString() : "")
                .withMessage(new EvaluationResult.Message("Hallo"))
                .build();
    }

    public static EvaluationResult evaluateValueDsl(Map<String, Object> context) {
        return EvaluationResult.builder(context.get("expected").equals(context.get("actual")))
                .withDslExpected("{5, 5}")
                .withDslActual("{4, 4}")
                .withMessage(new EvaluationResult.Message("Hallo"))
                .build();
    }
}
