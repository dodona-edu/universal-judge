public class Evaluator {

    public static EvaluationResult evaluate(Object actual) {
        if (actual instanceof ArithmeticException) {
            return EvaluationResult.builder(true)
                    .withReadableExpected(actual.toString())
                    .withReadableActual(actual.toString())
                    .build();
        } else {
            return EvaluationResult.builder(false)
                    .withReadableExpected("ArithmeticException")
                    .withReadableActual(actual == null ? "" : actual.toString())
                    .withMessage(new EvaluationResult.Message("Expected ArithmeticException, got something else."))
                    .build();
        }
    }

}