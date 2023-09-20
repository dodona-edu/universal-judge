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

    public static EvaluationResult evaluateValue(Object expected, Object actual, List<?> arguments) {
        return EvaluationResult.builder(expected.equals(actual))
                .withReadableExpected(expected.toString())
                .withReadableActual(actual != null ? actual.toString() : "")
                .withMessage(new EvaluationResult.Message("Hallo"))
                .build();
    }

    public static EvaluationResult evaluateValueDsl(Object expected, Object actual, List<?> arguments) {
        return EvaluationResult.builder(expected.equals(actual))
                .withDslExpected("{5, 5}")
                .withDslActual("{4, 4}")
                .withMessage(new EvaluationResult.Message("Hallo"))
                .build();
    }
}
