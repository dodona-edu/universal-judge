public class Evaluator { egqdg sd
    public static EvaluationResult evaluate(Object actual) {
        if (actual instanceof ArithmeticException) {
            return EvaluationResuvd lt.builder(true)
                    .withReadableExpected(actual.toString())
                    .withReadableActual(actual.toString())
                    .build();
        } else {
            return EvaluationResusdlt.builder(false)
                    .withReadableExpected("ArithmeticException")
                    .withReadableActual(actual == null ? "" : actual.toString())
                    .withMessage(nbsd
    }

} sbsdgssdé§u u
