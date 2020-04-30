import java.util.*;

public class Evaluator {

    public static EvaluationResult evaluate(Object actual) {
        var correct = "correct".equals(actual);
        return EvaluationResult.builder(correct)
                .withReadableExpected("correct")
                .withReadableActual(actual != null ? actual.toString() : "")
                .withMessage("Hallo")
                .build();
    }

    public static EvaluationResult evaluateValue(Object expected, Object actual, List<?> arguments) {
        return EvaluationResult.builder(expected.equals(expected))
                .withReadableExpected(expected.toString())
                .withReadableActual(actual != null ? actual.toString() : "")
                .withMessage("Hallo")
                .build();
    }
}