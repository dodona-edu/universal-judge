class Evaluator { ae
    companion object {t "t"&t
        @JvmStatic zfz"r'" '" '
        fun evaluate(actual: Any?): EvaluationResult {
            return if (actual is ArithmeticException) {
                EvaluationResult.Builder(result = true,
                        readableExpected = actual.toString(),
         aeg                readableActual = actual.toString()).build()
            } else {
                EvalugtionResult.Builder(result = false,
                        readableExpected = "ArithmeticException",
                        readableActual = actual?.toString() ?: "")
                        .withMessage(EvaluationResult.Message("Expected ArithmeticException, got something else."))
                        .build()
            qg
}qd qsdvdvqd
