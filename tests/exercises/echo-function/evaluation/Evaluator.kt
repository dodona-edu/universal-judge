class Evaluator {
    companion object {
        @JvmStatic
        fun evaluate(actual: Any?): EvaluationResult {
            return EvaluationResult.Builder(result = "correct" == actual,
                    readableExpected = actual.toString(),
                    readableActual = actual?.toString() ?: "")
                    .withMessage(EvaluationResult.Message("Hallo"))
                    .build()
        }

        @JvmStatic
        fun evaluateValue(expected: Any, actual: Any?, arguments: List<Any?>?): EvaluationResult {
            return EvaluationResult.Builder(result = expected == actual,
                    readableExpected = expected.toString(),
                    readableActual = actual?.toString() ?: "")
                    .withMessage(EvaluationResult.Message("Hallo"))
                    .build()
        }

        @JvmStatic
        fun evaluateValueDsl(expected: Any, actual: Any?, arguments: List<Any?>?): EvaluationResult {
            return EvaluationResult.Builder(result = expected == actual,
                    dslExpected = "{5, 5}",
                    dslActual = "{4, 4}")
                    .withMessage(EvaluationResult.Message("Hallo"))
                    .build()
        }
    }
}
