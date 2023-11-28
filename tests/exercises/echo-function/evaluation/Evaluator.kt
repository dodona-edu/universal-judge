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
        fun evaluateValue(context: Map<String, Any>): EvaluationResult {
            return EvaluationResult.Builder(result = context["expected"] == context["actual"],
                    readableExpected = context["expected"].toString(),
                    readableActual = context["actual"]?.toString() ?: "")
                    .withMessage(EvaluationResult.Message("Hallo"))
                    .build()
        }

        @JvmStatic
        fun evaluateValueDsl(context: Map<String, Any>): EvaluationResult {
            return EvaluationResult.Builder(result = context["expected"] == context["actual"],
                    dslExpected = "{5, 5}",
                    dslActual = "{4, 4}")
                    .withMessage(EvaluationResult.Message("Hallo"))
                    .build()
        }
    }
}
