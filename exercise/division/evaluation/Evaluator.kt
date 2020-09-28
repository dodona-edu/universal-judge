class Evaluator {
    companion object {
        @JvmStatic
        fun evaluate(actual: Any?): EvaluationResult {
            return if (actual is ArithmeticException) {
                EvaluationResult.Builder(result = true,
                        readableExpected = actual.toString(),
                        readableActual = actual.toString()).build()
            } else {
                EvaluationResult.Builder(result = false,
                        readableExpected = "ArithmeticException",
                        readableActual = actual?.toString() ?: "")
                        .withMessage(EvaluationResult.Message("Expected ArithmeticException, got something else."))
                        .build()
            }
        }
    }
}