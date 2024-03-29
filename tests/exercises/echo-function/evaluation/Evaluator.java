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
}
